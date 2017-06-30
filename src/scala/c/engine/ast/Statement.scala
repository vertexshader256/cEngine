package c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import scala.collection.mutable.{ListBuffer, Stack}

object Statement {

  // 'node' must be a IASTCaseStatement or a IASTDefaultStatement
  def processSwitch(node: IASTNode): Seq[IASTNode] = {
    val codeToRun = new ListBuffer[IASTNode]()

    val siblings = node.getParent.getChildren

    var isSelfFound = false
    siblings.foreach { sib =>
      if (sib == node) {
        isSelfFound = true
      } else if (isSelfFound && !sib.isInstanceOf[IASTCaseStatement]) {
        codeToRun += sib
      }
    }

    codeToRun
  }

  def parse(statement: NodePath)(implicit state: State): PartialFunction[Direction, Seq[IASTNode]] = statement.node match {
    case breakStatement: IASTNullStatement =>
      PartialFunction.empty
    case breakStatement: IASTBreakStatement => {
      case Stage1 =>
        state.popFunctionContext
        state.context.pathStack.pop
        Seq()
    }
    case continueStatement: IASTContinueStatement => {
      case Stage1 =>

        if (!state.context.isContinuable) {
          // search for a continuable scope
          while (!state.context.isContinuable) {
            state.popFunctionContext
          }
        }

        // reset the pathstack
        while (state.context.pathStack.size > 1) {
          state.context.pathStack.pop
        }

        Seq()
    }
    case goto: IASTGotoStatement => {
      case Stage1 =>

        val functionScope = state.getFunctionScope

        if (functionScope.labels.exists { label => label._1 == goto.getName.getRawSignature }) {
          state.context.pathStack.clear()
          state.context.pathStack.pushAll(functionScope.labels.head._2.map{x => NodePath(x.node, x.direction)})
          Seq()
        } else {
          state.isGotoing = true
          state.gotoName = goto.getName.getRawSignature
          Seq()
        }
    }
    case label: IASTLabelStatement => {
      case Exiting =>
        val functionScope = state.getFunctionScope
        val backupPath = Stack[NodePath]()
        backupPath.pushAll(state.context.pathStack.map{x => NodePath(x.node, x.direction)}.reverse)
        backupPath.pop
        backupPath.push(NodePath(label.getNestedStatement, Stage1))

        val backupPath2 = Stack[NodePath]()
        backupPath2.pushAll(backupPath)

        val ok = (label.getName.getRawSignature, backupPath2)
        functionScope.labels += ok
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
    case switch: IASTSwitchStatement => {
      case Stage1 =>
        state.pushScope(new SwitchScope(List(), switch, state.context, state) {})
        state.context.pathStack.push(NodePath(switch, Stage2))
        Seq()
      case Stage2 =>
        Seq(switch.getControllerExpression, switch.getBody) // only process case and default statements
      case Exiting =>
        state.popFunctionContext
        Seq()
    }
    case default: IASTDefaultStatement => {
      case _ => Seq()
    }
    case caseStatement: IASTCaseStatement => {
      case Stage2 => Seq(caseStatement.getExpression)
      case Exiting =>
        val caseExpr = state.context.stack.pop.asInstanceOf[RValue].value
        val switchExpr = state.context.stack.head

        val resolved = (switchExpr match {
          case info @ LValue(_, _) => info.value
          case value @ RValue(_, _) => value
        }).value


        var scope = state.context

        while (!scope.isInstanceOf[SwitchScope]) {
          scope = scope.parent
        }

        if (caseExpr == resolved || scope.asInstanceOf[SwitchScope].isCaseFound) {
          scope.asInstanceOf[SwitchScope].isCaseFound = true
          Seq() // match found, proceed
        } else {
          val cases = caseStatement.getParent.getChildren.collect{case c: IASTCaseStatement => c
                                                                  case d: IASTDefaultStatement => d}
          if (cases.last != caseStatement) {
            state.context.pathStack.pop
            var popped: NodePath = null
            while (!state.context.pathStack.head.node.isInstanceOf[IASTCaseStatement] && !state.context.pathStack.head.node.isInstanceOf[IASTDefaultStatement]) {
              popped = state.context.pathStack.pop
            }
            state.context.pathStack.push(popped)
          } else {
            while (state.context.pathStack.size > 2) {
              state.context.pathStack.pop
            }
          }
          Seq()
        }
    }
    case doWhileLoop: IASTDoStatement => {
      case Stage1 =>
        state.pushScope(new LoopScope(List(), doWhileLoop, state.context, state) {})
        state.context.pathStack.push(NodePath(doWhileLoop, Stage2))
        Seq()
      case Stage2 => doWhileLoop.getChildren
      case PreLoop =>
        val shouldLoop = TypeHelper.resolveBoolean(state.context.stack.pop)

        if (shouldLoop) {
          statement.direction = Stage2
          doWhileLoop.getChildren
        } else {
          Seq()
        }
      case Exiting =>
        state.popFunctionContext
        Seq()
      case Gotoing =>
        state.nextGotoNode = Seq(doWhileLoop.getCondition, doWhileLoop)
        state.context.pathStack.pop
        Seq(doWhileLoop.getBody)
    }
    case whileLoop: IASTWhileStatement => {
      case Stage1 =>
        state.pushScope(new LoopScope(List(), whileLoop, state.context, state) {})
        state.context.pathStack.push(NodePath(whileLoop, Stage2))
        Seq()
      case Stage2 => Seq(whileLoop.getCondition)
      case PreLoop =>
        val cast = state.context.stack.pop

        val shouldLoop = TypeHelper.resolveBoolean(cast)

        if (shouldLoop) {
          statement.direction = Stage1
          Seq(whileLoop.getBody)
        } else {
          Seq()
        }
      case Exiting =>
        state.popFunctionContext
        Seq()
      case Gotoing =>
        state.nextGotoNode = Seq(whileLoop.getCondition, whileLoop)
        state.context.pathStack.pop
        Seq(whileLoop.getBody)
    }
    case ifStatement: IASTIfStatement => {
      case Stage2 => Seq(ifStatement.getConditionExpression)
      case Exiting =>
        val result = state.context.stack.pop

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
    case forLoop: IASTForStatement => {
      case Stage1 =>
        state.pushScope(new LoopScope(List(), forLoop, state.context, state) {})
        state.context.pathStack.push(NodePath(forLoop, Stage2))
        Seq(Option(forLoop.getInitializerStatement)).flatten
      case Stage2 =>
        val shouldKeepLooping = if (forLoop.getConditionExpression != null) {
          val result = TypeHelper.resolve(Expressions.evaluate(forLoop.getConditionExpression).get).value
          TypeHelper.resolveBoolean(result)
        } else {
          // if theres no condition, always loop
          true
        }

        if (shouldKeepLooping) {
          Seq(forLoop.getBody)
        } else {
          statement.direction = Stage3
          Seq()
        }
      case Stage3 =>
        Expressions.evaluate(forLoop.getIterationExpression)
        statement.direction = Stage1
        Seq()
      case Exiting =>
        state.popFunctionContext
        Seq()
      case Gotoing =>
        state.nextGotoNode = Seq(forLoop.getBody, forLoop.getIterationExpression)
        statement.direction = Stage2
        Seq(forLoop.getBody, forLoop.getIterationExpression)
    }
    case ret: IASTReturnStatement => {
      case Stage1 =>
        if (ret.getReturnValue != null) {
          Seq(ret.getReturnValue)
        } else {
          Seq()
        }
      case Stage2 =>

        var retVal: ValueType = null

        if (ret.getReturnValue != null) {
          val returnVal = state.context.stack.pop

          retVal = returnVal match {
            case info @ LValue(addr, theType) =>
              val functionScope = state.getFunctionScope
              TypeHelper.cast(functionScope.function.getReturnType, info.value.value)
            case value @ RValue(_, _) => value
          }
        }

        while (state.numScopes > 1 && !state.context.isInstanceOf[FunctionScope]) {
          state.popFunctionContext
        }
        while (state.context.pathStack.size > 1) {
          state.context.pathStack.pop
        }

        if (retVal != null) {
          state.context.stack.push(retVal)
        }

        Seq()
    }
    case decl: IASTDeclarationStatement => {
      case Stage1 => Seq(decl.getDeclaration)
    }
    case compound: IASTCompoundStatement => {
      case Stage1 => compound.getStatements
      case Gotoing => compound.getStatements
    }
    case exprStatement: IASTExpressionStatement => {
      case Stage2 => Seq(exprStatement.getExpression)
      case Gotoing => Seq(exprStatement.getExpression)
    }
  }
}
