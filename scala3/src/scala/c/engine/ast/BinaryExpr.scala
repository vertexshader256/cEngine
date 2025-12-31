package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._
import org.eclipse.cdt.internal.core.dom.parser.c.CPointerType

object BinaryExpr {

	def evaluatePointerArithmetic(ptr: ValueType, offset: Int, operator: Int)(implicit state: State): RValue = {
		val rValue = TypeHelper.resolve(ptr)

		// For some reason double pointers should only use sizeof().  Not sure why.
		val theType = if ptr.isInstanceOf[LValue] && !TypeHelper.getPointerType(ptr.theType).isInstanceOf[CPointerType] then
			TypeHelper.getPointerType(ptr.theType)
		else
			ptr.theType

		val value = TypeHelper.sizeof(theType) * offset
		val bias = if (operator == `op_minus`) -1 else 1

		val computedOffset = value * bias

		Address(rValue.value.asInstanceOf[Int] + computedOffset, rValue.theType)
	}
	
	def evaluate(x: ValueType, y: ValueType, operator: Int)(implicit state: State): RValue = {
		val left = TypeHelper.resolve(x)
		val right = TypeHelper.resolve(y)

		val isLeftPointer = TypeHelper.isPointerOrArray(x)
		val isRightPointer = TypeHelper.isPointerOrArray(y)

		if (isLeftPointer && (operator == op_minus || operator == op_plus)) {
			val rightValue = TypeHelper.cast(TypeHelper.intType, right.value).value.asInstanceOf[Int]
			
			if (isRightPointer) {
				val leftSize = TypeHelper.sizeof(right.theType)
				val result = (left.value.asInstanceOf[Int] - rightValue) / leftSize
				Address(result, left.theType)
			} else {
				evaluatePointerArithmetic(left, rightValue, operator)
			}
		} else if (isRightPointer && operator == op_plus) {
			val leftValue = TypeHelper.cast(TypeHelper.intType, left.value).value.asInstanceOf[Int]
			val rightPtrSize = TypeHelper.sizeof(right.theType)
			val result = leftValue * rightPtrSize + right.value.asInstanceOf[Int]
			Address(result, right.theType)
		} else {
			if right.isInstanceOf[FileRValue] then
				right
			else
				val result = calculate(left.value, right.value, operator)
				RValue(result, left.theType)
		}
	}

	private def calculate(left: AnyVal, right: AnyVal, operator: Int)(implicit state: State): AnyVal = {
		// Because of integer promotion, C never does math on anything less than int's

		val op1 = left match
			case theChar: char => theChar.toInt
			case theShort: short => theShort.toInt
			case x => x

		val op2 = right match
			case theChar: char => theChar.toInt
			case theShort: short => theShort.toInt
			case x => x

		op1 match
			case x: Int => calculateInt(x, operator, op1, op2)
			case x: Long => calculateLong(x, operator, op1, op2)
			case x: Double => calculateDouble(x, operator, op1, op2)
			case x: Float => calculateFloat(x, operator, op1, op2)
			case _ => calculateBoolean(op1, op2, operator)
	}
	
	private def calculateBoolean(left: AnyVal, right: AnyVal, operator: Int): Boolean = {
		operator match
			case `op_greaterThan` =>
				greaterThan(left, right)
			case `op_logicalAnd` =>
				TypeHelper.resolveBoolean(left) && TypeHelper.resolveBoolean(right)
			case `op_logicalOr` =>
				TypeHelper.resolveBoolean(left) || TypeHelper.resolveBoolean(right)
			case `op_equals` =>
				left == right
			case `op_notequals` =>
				!calculateBoolean(left, right, op_equals)
			case `op_greaterEqual` =>
				calculateBoolean(left, right, op_greaterThan) || calculateBoolean(left, right, op_equals)
			case `op_lessThan` =>
				!calculateBoolean(left, right, op_greaterEqual)
			case `op_lessEqual` =>
				!calculateBoolean(left, right, op_greaterThan)
	}

	private def calculateInt(x: Int, operator: Int, op1: AnyVal, op2: AnyVal)(implicit state: State): AnyVal = {
		operator match
			case `op_assign` =>
				op2
			case `op_multiply` | `op_multiplyAssign` =>
				multiply(x, op2)
			case `op_plus` | `op_plusAssign` =>
				add(x, op2)
			case `op_minus` | `op_minusAssign` =>
				subtract(x, op2)
			case `op_divide` | `op_divideAssign` =>
				divide(x, op2)
			case `op_shiftRight` | `op_shiftRightAssign` =>
				x >> op2.asInstanceOf[Int]
			case `op_shiftLeft` | `op_shiftLeftAssign` =>
				x << op2.asInstanceOf[Int]
			case `op_modulo` =>
				op2 match
					case y: Long => x % y
					case y: Int => x % y
			case `op_binaryOr` | `op_binaryOrAssign` =>
				op2 match
					case y: Int => x | y
					case y: Long => x | y
			case `op_binaryXor` | `op_binaryXorAssign` =>
				op2 match
					case y: Int => x ^ y
					case y: Long => x ^ y
			case `op_binaryAnd` | `op_binaryAndAssign` =>
				op2 match
					case y: Int => x & y
					case y: Long => x & y
			case _ =>
				calculateBoolean(op1, op2, operator)
	}

	private def calculateLong(x: Long, operator: Int, op1: AnyVal, op2: AnyVal): AnyVal = {
		operator match
			case `op_assign` =>
				op2
			case `op_multiply` | `op_multiplyAssign` =>
				multiply(x, op2)
			case `op_plus` | `op_plusAssign` =>
				add(x, op2)
			case `op_minus` | `op_minusAssign` =>
				subtract(x, op2)
			case `op_divide` | `op_divideAssign` =>
				divide(x, op2)
			case `op_shiftRight` | `op_shiftRightAssign` =>
				x >> op2.asInstanceOf[Int]
			case `op_shiftLeft` | `op_shiftLeftAssign` =>
				x << op2.asInstanceOf[Int]
			case `op_modulo` =>
				op2 match
					case y: Long => x % y
					case y: Int => x % y
			case `op_binaryOr` | `op_binaryOrAssign` =>
				op2 match
					case y: Int => x | y
					case y: Long => x | y
			case `op_binaryXor` | `op_binaryXorAssign` =>
				op2 match
					case y: Int => x ^ y
					case y: Long => x ^ y
			case `op_binaryAnd` | `op_binaryAndAssign` =>
				op2 match
					case y: Int => x & y
					case y: Long => x & y
			case _ =>
				calculateBoolean(op1, op2, operator)
	}

	private def calculateDouble(x: Double, operator: Int, op1: AnyVal, op2: AnyVal): AnyVal = {
		operator match
			case `op_assign` =>
				op2
			case `op_multiply` | `op_multiplyAssign` =>
				multiply(x, op2)
			case `op_plus` | `op_plusAssign` =>
				add(x, op2)
			case `op_minus` | `op_minusAssign` =>
				subtract(x, op2)
			case `op_divide` | `op_divideAssign` =>
				divide(x, op2)
			case _ =>
				calculateBoolean(op1, op2, operator)
	}

	private def calculateFloat(x: Float, operator: Int, op1: AnyVal, op2: AnyVal): AnyVal = {
		operator match
			case `op_assign` =>
				op2
			case `op_multiply` | `op_multiplyAssign` =>
				multiply(x, op2)
			case `op_plus` | `op_plusAssign` =>
				add(x, op2)
			case `op_minus` | `op_minusAssign` =>
				subtract(x, op2)
			case `op_divide` | `op_divideAssign` =>
				divide(x, op2)
			case _ =>
				calculateBoolean(op1, op2, operator)
	}

	private def multiply[T](num1: AnyVal, num2: AnyVal) = {
		num1 match
			case x: Double =>
				num2 match
					case y: Int => x * y
					case y: Float => x * y
					case y: Double => x * y
					case y: Long => x * y
			case x: Float =>
				num2 match
					case y: Int => x * y
					case y: Float => x * y
					case y: Double => x * y
					case y: Long => x * y
			case x: Long =>
				num2 match
					case y: Int => x * y
					case y: Float => x * y
					case y: Double => x * y
					case y: Long => x * y
			case x: Int =>
				num2 match
					case y: Int => x * y
					case y: Float => x * y
					case y: Double => x * y
					case y: Long => x * y
	}

	private def add[T](num1: AnyVal, num2: AnyVal) = {
		num1 match
			case x: Double =>
				num2 match
					case y: Int => x + y
					case y: Float => x + y
					case y: Double => x + y
					case y: Long => x + y
			case x: Float =>
				num2 match
					case y: Int => x + y
					case y: Float => x + y
					case y: Double => x + y
					case y: Long => x + y
			case x: Long =>
				num2 match
					case y: Int => x + y
					case y: Float => x + y
					case y: Double => x + y
					case y: Long => x + y
			case x: Int =>
				num2 match
					case y: Int => x + y
					case y: Float => x + y
					case y: Double => x + y
					case y: Long => x + y
	}

	private def subtract[T](num1: AnyVal, num2: AnyVal) = {
		num1 match
			case x: Double =>
				num2 match
					case y: Int => x - y
					case y: Float => x - y
					case y: Double => x - y
					case y: Long => x - y
			case x: Float =>
				num2 match
					case y: Int => x - y
					case y: Float => x - y
					case y: Double => x - y
					case y: Long => x - y
			case x: Long =>
				num2 match
					case y: Int => x - y
					case y: Float => x - y
					case y: Double => x - y
					case y: Long => x - y
			case x: Int =>
				num2 match
					case y: Int => x - y
					case y: Float => x - y
					case y: Double => x - y
					case y: Long => x - y
	}

	private def divide[T](num1: AnyVal, num2: AnyVal) = {
		num1 match
			case x: Double =>
				num2 match
					case y: Int => x / y
					case y: Float => x / y
					case y: Double => x / y
					case y: Long => x / y
			case x: Float =>
				num2 match
					case y: Int => x / y
					case y: Float => x / y
					case y: Double => x / y
					case y: Long => x / y
			case x: Long =>
				num2 match
					case y: Int => x / y
					case y: Float => x / y
					case y: Double => x / y
					case y: Long => x / y
			case x: Int =>
				num2 match
					case y: Int => x / y
					case y: Float => x / y
					case y: Double => x / y
					case y: Long => x / y
	}

	private def greaterThan[T](num1: AnyVal, num2: AnyVal) = {
		num1 match
			case x: Double =>
				num2 match
					case y: Int => x > y
					case y: Float => x > y
					case y: Double => x > y
					case y: Long => x > y
			case x: Float =>
				num2 match
					case y: Int => x > y
					case y: Float => x > y
					case y: Double => x > y
					case y: Long => x > y
			case x: Long =>
				num2 match
					case y: Int => x > y
					case y: Float => x > y
					case y: Double => x > y
					case y: Long => x > y
			case x: Int =>
				num2 match
					case y: Int => x > y
					case y: Float => x > y
					case y: Double => x > y
					case y: Long => x > y
	}
}