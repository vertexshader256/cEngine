package c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import scala.collection.mutable.{ListBuffer, Stack}

object Statement {

  def parse(statement: IASTStatement)(implicit state: State) = statement match {
    case breakStatement: IASTNullStatement =>
      PartialFunction.empty
    case breakStatement: IASTBreakStatement => {
      while (!state.pathStack(state.pathIndex).isInstanceOf[BreakLabel]) {
        state.pathIndex += 1
      }
    }
    case continueStatement: IASTContinueStatement => {
      while (!state.pathStack(state.pathIndex).isInstanceOf[ContinueLabel]) {
        state.pathIndex -= 1
      }
    }
    case goto: IASTGotoStatement => {
      val functionScope = state.getFunctionScope

      if (functionScope.labels.exists { label => label._1 == goto.getName.getRawSignature }) {
        //state.context.pathStack.clear()
        //state.context.pathStack.pushAll(functionScope.labels.head._2.map{x => NodePath(x.node, x.direction)})
        Seq()
      } else {
        state.isGotoing = true
        state.gotoName = goto.getName.getRawSignature
        Seq()
      }
    }
    case label: IASTLabelStatement => {
//      case Exiting =>
//        val functionScope = state.getFunctionScope
//        val backupPath = Stack[NodePath]()
////        backupPath.pushAll(state.context.pathStack.map{x => NodePath(x.node, x.direction)}.reverse)
////        backupPath.pop
////        backupPath.push(NodePath(label.getNestedStatement, Stage1))
//
//        val backupPath2 = Stack[NodePath]()
//        backupPath2.pushAll(backupPath)
//
//        val ok = (label.getName.getRawSignature, backupPath2)
//        functionScope.labels += ok
//        Seq(label.getNestedStatement)
//      case Gotoing =>
//        if (label.getName.getRawSignature == state.gotoName) {
//          state.isGotoing = false
//          state.gotoName = ""
//          Seq(label.getNestedStatement) ++ state.nextGotoNode
//        } else {
//          Seq(label.getNestedStatement)
//        }
    }
    case switch: IASTSwitchStatement => {
//      case Stage1 =>
//        state.pushScope(new SwitchScope(List(), switch, state.context, state) {})
//        //state.context.pathStack.push(NodePath(switch, Stage2))
//        Seq()
//      case Stage2 =>
//        Seq(switch.getControllerExpression, switch.getBody) // only process case and default statements
//      case Exiting =>
//        state.popFunctionContext
//        Seq()
    }
    case default: IASTDefaultStatement => {
      Seq()
    }
    case caseStatement: IASTCaseStatement => {
      val caseExpr = Expressions.evaluate(caseStatement.getExpression).head.asInstanceOf[RValue].value
      val switchExpr = state.context.stack.head

      val resolved = (switchExpr match {
        case info @ LValue(_, _) => info.value
        case value @ RValue(_, _) => value
      }).value


      var scope = state.context

      while (!scope.isInstanceOf[SwitchScope]) {
        scope = scope.parent
      }

//        if (caseExpr == resolved || scope.asInstanceOf[SwitchScope].isCaseFound) {
//          scope.asInstanceOf[SwitchScope].isCaseFound = true
//          Seq() // match found, proceed
//        } else {
//          val cases = caseStatement.getParent.getChildren.collect{case c: IASTCaseStatement => c
//                                                                  case d: IASTDefaultStatement => d}
//          if (cases.last != caseStatement) {
//            state.context.pathStack.pop
//            var popped: NodePath = null
//            while (!state.context.pathStack.head.node.isInstanceOf[IASTCaseStatement] && !state.context.pathStack.head.node.isInstanceOf[IASTDefaultStatement]) {
//              popped = state.context.pathStack.pop
//            }
//            state.context.pathStack.push(popped)
//          } else {
//            while (state.context.pathStack.size > 2) {
//              state.context.pathStack.pop
//            }
//          }
//          Seq()
//        }
      Seq()
    }
    case doWhileLoop: IASTDoStatement => {
//      case Stage1 =>
//        state.pushScope(new LoopScope(List(), doWhileLoop, state.context, state) {})
//        //state.context.pathStack.push(NodePath(doWhileLoop, Stage2))
//        Seq()
//      case Stage2 => doWhileLoop.getChildren
//      case PreLoop =>
//        val shouldLoop = TypeHelper.resolveBoolean(state.context.stack.pop)
//
//        if (shouldLoop) {
//          //statement.direction = Stage2
//          doWhileLoop.getChildren
//        } else {
//          Seq()
//        }
//      case Exiting =>
//        state.popFunctionContext
//        Seq()
//      case Gotoing =>
//        state.nextGotoNode = Seq(doWhileLoop.getCondition, doWhileLoop)
//        //state.context.pathStack.pop
//        Seq(doWhileLoop.getBody)
    }
    case ret: IASTReturnStatement => {
      var retVal: ValueType = null

      if (ret.getReturnValue != null) {
        val returnVal = Expressions.evaluate(ret.getReturnValue).head

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
//        while (state.context.pathStack.size > 1) {
//          state.context.pathStack.pop
//        }

      if (retVal != null) {
        state.context.stack.push(retVal)
      }

      Seq()
    }
    case decl: IASTDeclarationStatement => {
      Seq(decl.getDeclaration)
    }
    case compound: IASTCompoundStatement => {
      compound.getStatements
    }
    case exprStatement: IASTExpressionStatement => {
      Seq(exprStatement.getExpression)
    }
  }
}
