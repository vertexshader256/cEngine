package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.*

import scala.c.engine.models.*

object Expressions {

	def evaluateAndResolveVariable(expr: IASTInitializerClause)(implicit state: State): ValueType = {
		evaluate(expr).get match
			case vari: Variable => vari.rValue
			case x => x
	}

	def evaluate(expr: IASTInitializerClause)(implicit state: State): Option[ValueType] = expr match {
		case exprList: IASTExpressionList =>
			exprList.getExpressions.map(evaluate).last
		case ternary: IASTConditionalExpression =>
			val result = TypeHelper.resolveBoolean(evaluate(ternary.getLogicalConditionExpression).get)

			val expr = if result then
				ternary.getPositiveResultExpression
			else
				ternary.getNegativeResultExpression

			evaluate(expr)
		case cast: IASTCastExpression =>
			Some(castExpression(cast))
		case fieldRef: IASTFieldReference =>
			Some(fieldReference(fieldRef))
		case subscript: IASTArraySubscriptExpression =>
			Some(arraySubscriptExpression(subscript))
		case unary: IASTUnaryExpression =>
			Some(UnaryExpression.execute(unary))
		case lit: IASTLiteralExpression =>
			Some(Literal.parse(lit.toString))
		case id: IASTIdExpression =>
			Some(state.context.resolveId(id.getName).get)
		case typeExpr: IASTTypeIdExpression =>
			// used for sizeof calls on a type
			val theType = TypeHelper.getType(typeExpr.getTypeId).theType
			Some(RValue(TypeHelper.sizeof(theType), TypeHelper.intType))
		case call: IASTFunctionCallExpression =>
			state.functionCallExpr(call)
		case bin: IASTBinaryExpression =>
			Some(binaryExpression(bin))
		case typeIdInit: IASTTypeIdInitializerExpression =>
			Some(typeExpr(typeIdInit))
	}

	private def castExpression(cast: IASTCastExpression)(implicit state: State): ValueType = {
		val theType = TypeHelper.getType(cast.getTypeId).theType
		val operand = evaluate(cast.getOperand).get

		operand match {
			case str @ StringLiteral(_) => str
			case LValue(addr, aType) =>
				val newAddr = state.allocateStack(TypeHelper.sizeof(theType))

				val value = theType match
					case ptr: IPointerType if aType.isInstanceOf[IArrayType] =>
						addr.location
					case _ =>
						val currentVal = state.memory.readFromMemory(addr, aType) // read current variable value
						TypeHelper.cast(currentVal.value, theType).value

				state.memory.writeToMemory(value, newAddr, theType) // write the casted data out
				LValue(state, newAddr, theType)
			case RValue(value, _) =>
				val newAddr = state.allocateStack(TypeHelper.sizeof(theType))
				state.memory.writeToMemory(TypeHelper.cast(value, theType).value, newAddr, theType)
				LValue(state, newAddr, theType)
		}
	}

	private def arraySubscriptExpression(subscript: IASTArraySubscriptExpression)(implicit state: State): LValue = {
		var left = evaluate(subscript.getArrayExpression).get
		var right = evaluate(subscript.getArgument).get

		val isLeftPointer = TypeHelper.isPointerOrArray(left)

		// in the case of weird stuff like 2[x], just swap the two operands
		if !isLeftPointer then
			val temp = left
			left = right
			right = temp

		val base = TypeHelper.toRValue(left).value.asInstanceOf[Int]

		val indexType = left match
			case RValue(_, theType) =>
				theType
			case LValue(_, theType) =>
				TypeHelper.getPointerType(theType)

		val rightValue = TypeHelper.toRValue(right).value
		val index = rightValue.toString.toInt
		val offset = base + index * TypeHelper.sizeof(indexType)

		LValue(state, Address(offset), indexType)
	}

	private def fieldReference(fieldRef: IASTFieldReference)(implicit state: State): Field = {
		val struct = evaluate(fieldRef.getFieldOwner).get.asInstanceOf[LValue]
		val structType = Structures.resolveStruct(struct.theType)

		val baseAddr = if fieldRef.isPointerDereference then
			Address(state.readPtrVal(struct.address))
		else
			struct.address

		Structures.offsetof(structType, baseAddr, fieldRef.getFieldName.toString, state)
	}

	private def binaryExpression(bin: IASTBinaryExpression)(implicit state: State): ValueType = {
		(bin.getOperator, evaluate(bin.getOperand1).head) match {
			case (IASTBinaryExpression.op_logicalOr, op1 @ RValue(x: Boolean, _)) if x => op1
			case (IASTBinaryExpression.op_logicalAnd, op1 @ RValue(x: Boolean, _)) if !x => op1
			case (_, op1) =>
				val op2 = evaluate(bin.getOperand2).get

				val result = if isAssignment(bin.getOperator) then {
					Declarator.assign(op1.asInstanceOf[LValue], List(op2), bin.getOperand2, bin.getOperator)
					op1
				} else
					BinaryExpr.evaluate(op1, op2, bin.getOperator)

				result
		}
	}

	private def typeExpr(typeIdInit: IASTTypeIdInitializerExpression)(implicit state: State): LValue = {
		val theType = TypeHelper.getType(typeIdInit.getTypeId).theType
		val newAddr = state.allocateStack(TypeHelper.sizeof(theType))

		typeIdInit.getInitializer match {
			case list: IASTInitializerList =>
				val rVals = list.getClauses.map { clause =>
					evaluate(clause).get match
						case r @ RValue(x, y) => r
						case l: LValue => l.rValue
				}.toList

				state.writeValues(newAddr, rVals)
		}

		LValue(state, newAddr, theType)
	}

	private def isAssignment(op: Int): Boolean = {
		op == op_assign ||
			op == op_plusAssign ||
			op == op_minusAssign ||
			op == op_multiplyAssign ||
			op == op_divideAssign ||
			op == op_moduloAssign ||
			op == op_binaryXorAssign ||
			op == op_binaryAndAssign ||
			op == op_binaryOrAssign ||
			op == op_multiplyAssign ||
			op == op_shiftLeftAssign ||
			op == op_shiftRightAssign
	}
}