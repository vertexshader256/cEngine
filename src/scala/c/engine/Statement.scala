package c.engine

import Executor.processSwitch
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.{CASTBreakStatement, CASTContinueStatement}

import scala.collection.mutable.Stack

object Statement {

  def parse(statement: IASTStatement, direction: Direction)(implicit state: State): Seq[IASTNode] = statement match {
    case breakStatement: IASTNullStatement =>
      Seq()
    case breakStatement: IASTBreakStatement =>
      val breakStatement = state.context.pathStack.pop.asInstanceOf[CASTBreakStatement]
      var reverse: IASTNode = breakStatement
      while ((!reverse.isInstanceOf[IASTWhileStatement] &&
        !reverse.isInstanceOf[IASTDoStatement] &&
        !reverse.isInstanceOf[IASTForStatement] &&
        !reverse.isInstanceOf[IASTSwitchStatement]) || !Utils.getAncestors(breakStatement).contains(reverse)) {
        reverse = state.context.pathStack.pop
      }
      Seq()
    case continueStatement: IASTContinueStatement =>
      val continueStatement = state.context.pathStack.pop.asInstanceOf[CASTContinueStatement]
      var last: IASTNode = continueStatement

      // find the first for loop that is a direct ancestor
      while (!last.isInstanceOf[IASTForStatement] || !Utils.getAncestors(continueStatement).contains(last)) {
        last = state.context.pathStack.pop
      }

      val forLoop = last.asInstanceOf[IASTForStatement]

      state.context.pathStack.push(forLoop)
      state.context.pathStack.push(forLoop.getConditionExpression)
      state.context.pathStack.push(forLoop.getIterationExpression)
      Seq()
    case goto: IASTGotoStatement =>
      if (state.context.labels.exists{label => label._1.getName.getRawSignature == goto.getName.getRawSignature}) {
        state.context.pathStack.clear()
        state.context.pathStack.pushAll(state.context.labels.head._2.reverse :+ state.context.labels.head._1)

        state.context.visited.clear()
        state.context.visited ++= state.context.labels.head._3
        Seq()
      } else {
        state.isGotoing = true
        state.gotoName = goto.getName.getRawSignature
        Seq()
      }
    case label: IASTLabelStatement => direction match {
      case Entering =>
        Seq()
      case Exiting =>
        val backupPath = Stack[IASTNode]() ++ state.context.pathStack
        val ok = (label, backupPath, state.context.visited.toList)
        state.context.labels += ok
        Seq(label.getNestedStatement)
      case Gotoing =>
        if (label.getName.getRawSignature == state.gotoName) {
          state.isGotoing = false
          state.gotoName = ""
          Seq(label.getNestedStatement) ++ state.nextGotoNode
        } else {
          Seq(label.getNestedStatement)
        }
    }
    case switch: IASTSwitchStatement => direction match {
      case Entering =>
        val cases = switch.getBody.getChildren.collect { case x: IASTCaseStatement => x; case y: IASTDefaultStatement => y }
        Seq(switch.getControllerExpression) ++ cases // only process case and default statements
      case Exiting => Seq()
    }
    case default: IASTDefaultStatement => direction match {
      case Entering => Seq()
      case Exiting => processSwitch(default)
    }
    case caseStatement: IASTCaseStatement => direction match {
      case Entering => Seq(caseStatement.getExpression)
      case Exiting =>
        val caseExpr = state.stack.pop.asInstanceOf[RValue].value
        val switchExpr = state.stack.head

        val resolved = (switchExpr match {
          case info @ LValue(_, _) => info.value
          case value @ RValue(_, _) => value
        }).value

        if (caseExpr == resolved) {
          processSwitch(caseStatement)
        } else {
          Seq()
        }
    }
    case doWhileLoop: IASTDoStatement => direction match {
      case Entering => Seq(doWhileLoop.getBody, doWhileLoop.getCondition)
      case Exiting =>
        val shouldLoop = TypeHelper.resolveBoolean(state.stack.pop)

        if (shouldLoop) {
          state.clearVisited(doWhileLoop.getBody)
          state.clearVisited(doWhileLoop.getCondition)

          Seq(doWhileLoop.getBody, doWhileLoop.getCondition, doWhileLoop)
        } else {
          Seq()
        }
      case Gotoing =>
        state.nextGotoNode = Seq(doWhileLoop.getCondition, doWhileLoop)
        state.context.pathStack.pop
        Seq(doWhileLoop.getBody)
    }
    case whileLoop: IASTWhileStatement => direction match {
      case Entering => Seq(whileLoop.getCondition)
      case Exiting =>
        val cast = state.stack.pop

        val shouldLoop = TypeHelper.resolveBoolean(cast)

        if (shouldLoop) {
          state.clearVisited(whileLoop.getBody)
          state.clearVisited(whileLoop.getCondition)

          Seq(whileLoop.getBody, whileLoop.getCondition, whileLoop)
        } else {
          Seq()
        }
      case Gotoing =>
        state.nextGotoNode = Seq(whileLoop.getCondition, whileLoop)
        state.context.pathStack.pop
        Seq(whileLoop.getBody)
    }
    case ifStatement: IASTIfStatement => direction match {
      case Entering => Seq(ifStatement.getConditionExpression)
      case Exiting =>
        val result = state.stack.pop

        val value = result match {
          case info @ LValue(_,_) => info.value
          case x => x
        }

        val conditionResult = TypeHelper.resolveBoolean(value)

        if (conditionResult) {
          Seq(ifStatement.getThenClause)
        } else if (ifStatement.getElseClause != null) {
          Seq(ifStatement.getElseClause)
        } else {
          Seq()
        }
      case Gotoing => Seq(ifStatement.getConditionExpression)
    }
    case forLoop: IASTForStatement => direction match {
      case Entering => Seq(Option(forLoop.getInitializerStatement), Option(forLoop.getConditionExpression)).flatten
      case Exiting =>
        val shouldKeepLooping = if (forLoop.getConditionExpression != null) {

          val result = TypeHelper.resolve(state.stack.pop).value

          TypeHelper.resolveBoolean(result)
        } else {
          true
        }

        if (shouldKeepLooping) {
          state.clearVisited(forLoop.getBody)
          state.clearVisited(forLoop.getIterationExpression)

          if (forLoop.getConditionExpression != null) {
            state.clearVisited(forLoop.getConditionExpression)
          }

          Seq(Option(forLoop.getBody), Option(forLoop.getIterationExpression), Option(forLoop.getConditionExpression), Some(forLoop)).flatten
        } else {
          Seq()
        }
      case Gotoing =>
        state.nextGotoNode = Seq(Option(forLoop.getBody), Option(forLoop.getIterationExpression), Option(forLoop.getConditionExpression), Some(forLoop)).flatten
        state.context.pathStack.pop
        Seq(forLoop.getBody)
    }
    case ret: IASTReturnStatement => direction match {
      case Entering =>
        if (ret.getReturnValue != null) {
          Seq(ret.getReturnValue)
        } else {
          Seq()
        }
      case Exiting =>
        if (ret.getReturnValue != null) {
          val returnVal = state.stack.pop
          state.stack.push(returnVal match {
            case info @ LValue(addr, theType) => TypeHelper.cast(state.context.returnType, info.value.value)
            case value @ RValue(_, _) => value
          })
        }
        state.isReturning = true

        Seq()
    }
    case decl: IASTDeclarationStatement => direction match {
      case Entering => Seq(decl.getDeclaration)
      case Exiting => Seq()
    }
    case compound: IASTCompoundStatement => direction match {
      case Entering => compound.getStatements
      case Exiting => Seq()
      case Gotoing => compound.getStatements
    }
    case exprStatement: IASTExpressionStatement => direction match {
      case Entering => Seq(exprStatement.getExpression)
      case Exiting => Seq()
      case Gotoing => Seq(exprStatement.getExpression)
    }
  }
}
