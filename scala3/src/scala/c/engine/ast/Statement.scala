package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.CStructure

import scala.c.engine.Instructions.ReturnFromFunction
import scala.c.engine.models.*

object Statement {

	def step(statement: IASTStatement)(implicit state: CEngine): Unit = statement match {
		case _: IASTNullStatement =>
			PartialFunction.empty
		case ret: IASTReturnStatement =>
			if (ret.getReturnValue != null) {
				val returnVal = Expressions.evaluate(ret.getReturnValue).head
				val functionScope = state.getFunctionScope

				val retVal = returnVal match
					case structure @ LValue(addr, struct: CStructure) =>
						structure
					case info @ LValue(_, _) =>
						TypeHelper.cast(info.rValue.value, functionScope.returnType)
					case value @ RValue(_, _) if functionScope.returnType != null =>
						TypeHelper.cast(value.value, functionScope.returnType)
					case value @ RValue(_, _) => value
					case struct: Structure =>
						val newVar = state.context.addVariable("synthetic", struct.theType) // create a temp var
						state.writeDataBlock(newVar.address, struct.bytes)
						newVar

				state.context.pushOntoStack(retVal)
			}

			throw ReturnFromFunction()
		case decl: IASTDeclarationStatement =>
			Ast.step(decl.getDeclaration)
		case exprStatement: IASTExpressionStatement =>
			Expressions.evaluate(exprStatement.getExpression)
	}
}
