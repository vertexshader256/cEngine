package c.engine

import Executor.processSwitch
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.{CASTBreakStatement, CASTContinueStatement, CFunctionType}

import scala.c.engine.{FunctionScope, NodePath, Scope}
import scala.collection.mutable.Stack

object Statement {

  def parse(statement: NodePath)(implicit state: State): PartialFunction[Direction, Seq[IASTNode]] = statement.node match {
    case breakStatement: IASTNullStatement =>
      PartialFunction.empty
    case breakStatement: IASTBreakStatement => {
      case Initial =>
        var reverse = state.context.pathStack.pop
        var shouldBreak = false
        while ((!shouldBreak && !reverse.node.isInstanceOf[IASTWhileStatement] &&
          !reverse.node.isInstanceOf[IASTDoStatement] &&
          !reverse.node.isInstanceOf[IASTForStatement])) {
          if (state.context.pathStack.head.node.isInstanceOf[IASTSwitchStatement]) {
            shouldBreak = true
          } else {
            reverse = state.context.pathStack.pop
          }
        }

        if (reverse.node.isInstanceOf[IASTDoStatement] || reverse.node.isInstanceOf[IASTWhileStatement]) {
          state.popFunctionContext
          state.context.pathStack.pop
        }

        if (reverse.node.isInstanceOf[IASTForStatement]) {
          state.popFunctionContext
          state.context.pathStack.pop
        }
        Seq()
    }
    case continueStatement: IASTContinueStatement => {
      case Initial =>
        val continueStatement = state.context.pathStack.pop.node
        var last: NodePath = statement

        // find the first for loop that is a direct ancestor
        while (!last.node.isInstanceOf[IASTForStatement] || !Utils.getAncestors(continueStatement).contains(last.node)) {
          last = state.context.pathStack.pop
        }

        val forLoop = last.node.asInstanceOf[IASTForStatement]

        state.context.pathStack.push(last)
        last.direction = Entering

        Seq(Option(forLoop.getIterationExpression)).flatten
    }
    case goto: IASTGotoStatement => {
      case Initial =>

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
        backupPath.push(NodePath(label.getNestedStatement, Initial))

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
      case Entering =>
        val cases = switch.getBody.getChildren.collect { case x: IASTCaseStatement => x; case y: IASTDefaultStatement => y }
        Seq(switch.getControllerExpression) ++ cases // only process case and default statements
    }
    case default: IASTDefaultStatement => {
      case Exiting => processSwitch(default)
    }
    case caseStatement: IASTCaseStatement => {
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
    case doWhileLoop: IASTDoStatement => {
      case Initial =>
        state.functionContexts.push(new Scope(List(), state.functionContexts.head.varMap, state) {})
        state.context.pathStack.push(NodePath(doWhileLoop, Entering))
        Seq()
      case Entering => Seq(doWhileLoop.getBody, doWhileLoop.getCondition)
      case PreExit =>
        val shouldLoop = TypeHelper.resolveBoolean(state.stack.pop)

        if (shouldLoop) {
          statement.direction = Entering
          Seq(doWhileLoop.getBody, doWhileLoop.getCondition)
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
      case Initial =>
        state.functionContexts.push(new Scope(List(), state.functionContexts.head.varMap, state) {})
        state.context.pathStack.push(NodePath(whileLoop, Entering))
        Seq()
      case Entering => Seq(whileLoop.getCondition)
      case PreExit =>
        val cast = state.stack.pop

        val shouldLoop = TypeHelper.resolveBoolean(cast)

        if (shouldLoop) {
          statement.direction = Initial
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
    case forLoop: IASTForStatement => {
      case Initial =>
        state.functionContexts.push(new Scope(List(), state.functionContexts.head.varMap, state) {})
        state.context.pathStack.push(NodePath(forLoop, Entering))
        Seq(Option(forLoop.getInitializerStatement)).flatten
      case Entering => Seq(Option(forLoop.getConditionExpression)).flatten
      case PreExit =>
        val shouldKeepLooping = if (forLoop.getConditionExpression != null) {

          val result = TypeHelper.resolve(state.stack.pop).value

          TypeHelper.resolveBoolean(result)
        } else {
          true
        }

        if (shouldKeepLooping) {
          statement.direction = Initial
          Seq(Option(forLoop.getBody), Option(forLoop.getIterationExpression)).flatten
        } else {
          Seq()
        }
      case Exiting =>
        state.popFunctionContext
        Seq()
      case Gotoing =>
        state.nextGotoNode = Seq(Option(forLoop.getBody), Option(forLoop.getIterationExpression)).flatten
        statement.direction = Entering
        Seq(forLoop.getBody, forLoop.getIterationExpression)
    }
    case ret: IASTReturnStatement => {
      case Initial =>
        if (ret.getReturnValue != null) {
          Seq(ret.getReturnValue)
        } else {
          Seq()
        }
      case Entering =>

        var retVal: ValueType = null

        if (ret.getReturnValue != null) {
          val returnVal = state.stack.pop

          retVal = returnVal match {
            case info @ LValue(addr, theType) =>
              val functionScope = state.getFunctionScope
              TypeHelper.cast(functionScope.function.getReturnType, info.value.value)
            case value @ RValue(_, _) => value
          }
        }

        if (state.functionContexts.size > 1) {

          var currentScope: Scope = null
          do {
            currentScope = state.functionContexts.head
            state.popFunctionContext
            state.context.pathStack.pop
          } while (state.functionContexts.size > 1 && !currentScope.isInstanceOf[FunctionScope])
        }

        if (retVal != null) {
          state.context.stack.push(retVal)
        }

        Seq()
    }
    case decl: IASTDeclarationStatement => {
      case Initial => Seq(decl.getDeclaration)
    }
    case compound: IASTCompoundStatement => {
      case Initial => compound.getStatements
      case Gotoing => compound.getStatements
    }
    case exprStatement: IASTExpressionStatement => {
      case Entering => Seq(exprStatement.getExpression)
      case Gotoing => Seq(exprStatement.getExpression)
    }
  }
}
