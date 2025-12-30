package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.anarres.cpp.{InputLexerSource, Preprocessor, Token}
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.*
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.parser.{DefaultLogService, FileContent, IncludeFileContentProvider, ScannerInfo}
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType

import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets
import java.util
import java.util.HashMap
import scala.collection.mutable.ListBuffer

object Expressions {

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
			val theType = TypeHelper.getType(cast.getTypeId).theType
			val operand = evaluate(cast.getOperand).get

			Some(operand match {
				case str@StringLiteral(_) => str
				case LValue(addr, aType) =>
					theType match {
						case ptr: IPointerType if aType.isInstanceOf[IArrayType] =>
							val newAddr = state.allocateSpace(TypeHelper.sizeof(theType))
							state.Stack.writeToMemory(addr, newAddr, theType)
							LValue(state, newAddr, theType)
						case _ =>
							val currentVal = state.Stack.readFromMemory(addr, aType) // read current variable value
							val casted = TypeHelper.cast(theType, currentVal.value).value
							val newAddr = state.allocateSpace(TypeHelper.sizeof(theType))
							state.Stack.writeToMemory(casted, newAddr, theType) // write the casted data out
							LValue(state, newAddr, theType)
					}
				case RValue(value, _) =>
					val newAddr = state.allocateSpace(TypeHelper.sizeof(theType))
					state.Stack.writeToMemory(TypeHelper.cast(theType, value).value, newAddr, theType)
					LValue(state, newAddr, theType)
			})
		case fieldRef: IASTFieldReference =>
			val struct = evaluate(fieldRef.getFieldOwner).get.asInstanceOf[LValue]

			val structType = TypeHelper.resolveStruct(struct.theType)

			val baseAddr = if fieldRef.isPointerDereference then
				state.readPtrVal(struct.address)
			else
				struct.address

			val field = TypeHelper.offsetof(structType, baseAddr, fieldRef.getFieldName.toString, state: State)
			Some(field)
		case subscript: IASTArraySubscriptExpression =>

			var left = evaluate(subscript.getArrayExpression).get
			var right = evaluate(subscript.getArgument).get

			val isLeftPointer = TypeHelper.isPointerOrArray(left)

			// in the case of weird stuff like 2[x], just swap the two operands
			if !isLeftPointer then
				val temp = left
				left = right
				right = temp

			val base = TypeHelper.resolve(left).value.asInstanceOf[Int]

			val indexType = left match
				case RValue(_, theType) =>
					theType
				case LValue(_, theType) =>
					TypeHelper.getPointerType(theType)

			val rightValue = TypeHelper.resolve(right).value
			val index = rightValue.toString.toInt
			val offset = base + index * TypeHelper.sizeof(indexType)

			Some(LValue(state, offset, indexType))
		case unary: IASTUnaryExpression =>
			Some(UnaryExpression.execute(unary))
		case lit: IASTLiteralExpression =>
			Some(Literal.cast(lit.toString))
		case id: IASTIdExpression =>
			Some(state.context.resolveId(id.getName).get)
		case typeExpr: IASTTypeIdExpression =>
			// used for sizeof calls on a type
			val theType = TypeHelper.getType(typeExpr.getTypeId).theType
			Some(RValue(TypeHelper.sizeof(theType), TypeHelper.intType))
		case call: IASTFunctionCallExpression =>
			val pop = evaluate(call.getFunctionNameExpression).head

			val name = if (state.hasFunction(call.getFunctionNameExpression.getRawSignature)) {
				call.getFunctionNameExpression.getRawSignature
			} else {
				val info = pop.asInstanceOf[LValue]
				val resolved = TypeHelper.stripSyntheticTypeInfo(info.theType)
				resolved match {
					case _: IPointerType => state.getFunctionByIndex(info.rValue.value.asInstanceOf[Int]).name
				}
			}

			state.callTheFunction(name, call, None)
		case bin: IASTBinaryExpression =>
			(bin.getOperator, evaluate(bin.getOperand1).head) match {
				case (IASTBinaryExpression.op_logicalOr, op1@RValue(x: Boolean, _)) if x => Some(op1)
				case (IASTBinaryExpression.op_logicalAnd, op1@RValue(x: Boolean, _)) if !x => Some(op1)
				case (_, op1) =>
					val op2 = evaluate(bin.getOperand2).head

					val result = if isAssignment(bin.getOperator) then
						Declarator.assign(op1.asInstanceOf[LValue], List(op2), bin.getOperand2, bin.getOperator)
					else
						BinaryExpr.evaluate(op1, op2, bin.getOperator)

					Some(result)
			}
		case typeIdInit: IASTTypeIdInitializerExpression =>
			val theType = TypeHelper.getType(typeIdInit.getTypeId).theType
			val newAddr = state.allocateSpace(TypeHelper.sizeof(theType))

			typeIdInit.getInitializer match {
				case list: IASTInitializerList =>
					val rVals = list.getClauses.map { clause =>
						evaluate(clause).get match
							case r@RValue(x, y) => r
							case l: LValue => l.rValue
					}.toList

					state.writeDataBlock(rVals, newAddr)
			}

			Some(LValue(state, newAddr, theType))
	}

	private def isAssignment(op: Int) = {
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