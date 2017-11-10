package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import scala.collection.mutable.{ListBuffer, Stack}

object Statement {

  def parse(statement: IASTStatement)(implicit state: State): Unit = statement match {
    case _: IASTNullStatement =>
      PartialFunction.empty
    case _: IASTContinueStatement => {
      while (!state.context.pathStack(state.context.pathIndex).isInstanceOf[ContinueLabel]) {
        state.context.pathIndex += 1
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

      if (retVal != null) {
        state.context.stack.push(retVal)
      }

      throw ReturnFromFunction()
    }
    case decl: IASTDeclarationStatement =>
      Ast.step(decl.getDeclaration)
    case exprStatement: IASTExpressionStatement => {
      Expressions.evaluate(exprStatement.getExpression)
    }
  }
}
