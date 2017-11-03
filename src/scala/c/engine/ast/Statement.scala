package c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import scala.collection.mutable.{ListBuffer, Stack}

object Statement {

  def parse(statement: IASTStatement)(implicit state: State) = statement match {
    case _: IASTNullStatement =>
      PartialFunction.empty
//    case _: IASTBreakStatement => {
//      while (!state.pathStack(state.pathIndex).isInstanceOf[BreakLabel]) {
//        state.pathIndex += 1
//      }
//    }
    case _: IASTContinueStatement => {
      while (!state.pathStack(state.pathIndex).isInstanceOf[ContinueLabel]) {
        state.pathIndex += 1
      }
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
