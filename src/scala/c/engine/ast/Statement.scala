package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import scala.collection.mutable.{ListBuffer, Stack}

object Statement {

  def parse(statement: IASTStatement)(implicit state: State): Unit = statement match {
    case _: IASTNullStatement =>
      PartialFunction.empty
    case ret: IASTReturnStatement => {
      var retVal: ValueType = null

      if (ret.getReturnValue != null) {
        val returnVal = Expressions.evaluate(ret.getReturnValue).head
        val functionScope = state.getFunctionScope

        retVal = returnVal match {
          case info @ LValue(addr, theType) =>
            TypeHelper.cast(functionScope.returnType, info.rValue.value)
          case value @ RValue(_, _) if functionScope.returnType != null =>
            TypeHelper.cast(functionScope.returnType, value.value)
          case value @ RValue(_, _) => value
        }
      }

      if (retVal != null) {
        state.context.pushOntoStack(List(retVal))
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
