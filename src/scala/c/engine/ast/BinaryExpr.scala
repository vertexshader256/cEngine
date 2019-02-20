package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.{CArrayType, CBasicType, CPointerType, CStructure}
import IBasicType.Kind._


object BinaryExpr {

  def calculateBoolean(left: AnyVal, right: AnyVal, operator: Int): Boolean = operator match {
    case `op_greaterThan` =>
      (left, right) match {
        case (x: Int, y: Int) => x > y
        case (x: Int, y: Float) => x > y
        case (x: Int, y: Double) => x > y
        case (x: Int, y: Long) => x > y

        case (x: Float, y: Int) => x > y
        case (x: Float, y: Double) => x > y
        case (x: Float, y: Float) => x > y
        case (x: Float, y: Long) => x > y

        case (x: Double, y: Int) => x > y
        case (x: Double, y: Double) => x > y
        case (x: Double, y: Float) => x > y
        case (x: Double, y: Long) => x > y

        case (x: Long, y: Int) => x > y
        case (x: Long, y: Float) => x > y
        case (x: Long, y: Double) => x > y
        case (x: Long, y: Long) => x > y
      }
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

  def evaluatePointerArithmetic(left: RValue, offset: Int, operator: Int)(implicit state: State): RValue = {
    val ptrSize = TypeHelper.sizeof(left.theType)
    println(ptrSize)
    val value = offset * ptrSize
    val computedOffset = if (operator == `op_minus`) {
      -value
    } else {
      +value
    }

    Address(left.value.asInstanceOf[Int] + computedOffset, left.theType)
  }

  def calculate(left: AnyVal, right: AnyVal, operator: Int)(implicit state: State): AnyVal = {
    // Because of integer promotion, C never does math on anything less than int's

    val op1 = left match {
      case theChar: char => theChar.toInt
      case theShort: short => theShort.toInt
      case x => x
    }

    val op2 = right match {
      case theChar: char => theChar.toInt
      case theShort: short => theShort.toInt
      case x => x
    }

    operator match {
      case `op_multiply` | `op_multiplyAssign` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x * y
          case (x: Int, y: Float) => x * y
          case (x: Int, y: Double) => x * y
          case (x: Int, y: Long) => x * y

          case (x: Float, y: Int) => x * y
          case (x: Float, y: Double) => x * y
          case (x: Float, y: Float) => x * y

          case (x: Double, y: Int) => x * y
          case (x: Double, y: Double) => x * y
          case (x: Double, y: Float) => x * y

          case (x: Long, y: Int) => x * y
          case (x: Long, y: Float) => x * y
          case (x: Long, y: Double) => x * y
          case (x: Long, y: Long) => x * y
        }
      case `op_plus` | `op_plusAssign` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x + y
          case (x: Int, y: Float) => x + y
          case (x: Int, y: Double) => x + y
          case (x: Int, y: Long) => x + y

          case (x: Float, y: Int) => x + y
          case (x: Float, y: Float) => x + y
          case (x: Float, y: Double) => x + y
          case (x: Float, y: Long) => x + y

          case (x: Double, y: Int) => x + y
          case (x: Double, y: Double) => x + y
          case (x: Double, y: Float) => x + y
          case (x: Double, y: Long) => x + y

          case (x: Long, y: Int) => x + y
          case (x: Long, y: Float) => x + y
          case (x: Long, y: Double) => x + y
          case (x: Long, y: Long) => x + y
        }
      case `op_minus` | `op_minusAssign` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x - y
          case (x: Int, y: Float) => x - y
          case (x: Int, y: Double) => x - y
          case (x: Int, y: Long) => x - y

          case (x: Float, y: Int) => x - y
          case (x: Float, y: Double) => x - y
          case (x: Float, y: Float) => x - y

          case (x: Double, y: Int) => x - y
          case (x: Double, y: Double) => x - y
          case (x: Double, y: Float) => x - y

          case (x: Long, y: Int) => x - y
          case (x: Long, y: Float) => x - y
          case (x: Long, y: Double) => x - y
          case (x: Long, y: Long) => x - y
        }
      case `op_divide` | `op_divideAssign` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x / y
          case (x: Int, y: Float) => x / y
          case (x: Int, y: Double) => x / y
          case (x: Int, y: Long) => x / y

          case (x: Float, y: Int) => x / y
          case (x: Float, y: Double) => x / y
          case (x: Float, y: Float) => x / y
          case (x: Float, y: Long) => x / y

          case (x: Double, y: Int) => x / y
          case (x: Double, y: Double) => x / y
          case (x: Double, y: Float) => x / y
          case (x: Double, y: Long) => x / y

          case (x: Long, y: Int) => x / y
          case (x: Long, y: Float) => x / y
          case (x: Long, y: Double) => x / y
          case (x: Long, y: Long) => x / y
        }
      case `op_shiftRight` | `op_shiftRightAssign` =>
        (op1, op2) match {
          case (x: Long, y: Int) => x >> y
          case (x: Int, y: Int) => x >> y
        }
      case `op_shiftLeft` | `op_shiftLeftAssign` =>
        (op1, op2) match {
          case (x: Long, y: Int) => x << y
          case (x: Int, y: Int) => x << y
        }
      case `op_modulo` =>
        (op1, op2) match {
          case (x: Long, y: Long) => if (x % y >= 0) x % y else (x % y) + y
          case (x: Long, y: Int) => if (x % y >= 0) x % y else (x % y) + y

          case (x: Int, y: Int) => if (x % y >= 0) x % y else (x % y) + y
          case (x: Int, y: Double) => if (x % y >= 0) x % y else (x % y) + y
          case (x: Int, y: Long) => if (x % y >= 0) x % y else (x % y) + y

          case (x: Double, y: Int) => if (x % y >= 0) x % y else (x % y) + y
          case (x: Double, y: Double) => if (x % y >= 0) x % y else (x % y) + y
          case (x: Double, y: Long) => if (x % y >= 0) x % y else (x % y) + y
        }
      case `op_binaryOr`  | `op_binaryOrAssign`=>
        (op1, op2) match {
          case (x: Int, y: Int) => x | y
          case (x: Int, y: Long) => x | y
          case (x: Long, y: Int) => x | y
          case (x: Long, y: Long) => x | y
        }
      case `op_binaryXor` | `op_binaryXorAssign` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x ^ y
          case (x: Int, y: Long) => x ^ y
          case (x: Long, y: Int) => x ^ y
          case (x: Long, y: Long) => x ^ y
        }
      case `op_binaryAnd` | `op_binaryAndAssign` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x & y
          case (x: Int, y: Long) => x & y
          case (x: Long, y: Int) => x & y
          case (x: Long, y: Long) => x & y
        }
      case `op_assign` =>
        op2
      case _ =>
        calculateBoolean(op1, op2, operator)
    }
  }

  def evaluate(x: ValueType, y: ValueType, operator: Int)(implicit state: State): RValue = {
    val left = TypeHelper.resolve(x)
    val right = TypeHelper.resolve(y)

    if (TypeHelper.isPointer(x) || TypeHelper.isPointer(y)) {

      val isLeftPointer = TypeHelper.isPointer(left)
      val isRightPointer = TypeHelper.isPointer(right)

      if (operator == `op_minus` || operator == `op_plus`) {

        val ptrSize = TypeHelper.sizeof(left.theType)

        if (isLeftPointer && isRightPointer) {
          // assume minus
          val result = (left.value.asInstanceOf[Int] - right.value.asInstanceOf[Int]) / ptrSize
          RValue(result, new CPointerType(left.theType, 0))
        } else if (!isLeftPointer && isRightPointer) {
          // assume plus
          val rightPtrSize = TypeHelper.sizeof(right.theType)
          val result = left.value.asInstanceOf[Int] * rightPtrSize + right.value.asInstanceOf[Int]
          RValue(result, new CPointerType(right.theType, 0))
        } else {
          evaluatePointerArithmetic(left, right.value.asInstanceOf[Int], operator)
        }
      } else {
        RValue(calculate(left.value, right.value, operator), left.theType)
      }

    } else {

      // conversion rules
      val resultType = (left.theType, right.theType) match {
        case (l: IBasicType, r: IBasicType) => (l.getKind, r.getKind) match {
          case (`eDouble` | `eFloat`, `eDouble` | `eFloat`) => new CBasicType(eDouble, 0)
          case _ => TypeHelper.intType
        }
        case _ => left.theType
      }

      val result = calculate(left.value,  right.value, operator)
      RValue(result, resultType)
    }
  }
}