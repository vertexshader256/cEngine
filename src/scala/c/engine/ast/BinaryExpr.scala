package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.{CBasicType, CStructure}
import IBasicType.Kind._

object BinaryExpr {
  
  def parseAssign(node: IASTNode, op: Int, dst: LValue, op2: ValueType)(implicit state: State): LValue = {

    val result = evaluate(node, dst, op2, op)

    if (dst.theType.isInstanceOf[CStructure]) {

      val otherVar = op2.asInstanceOf[Variable]

      val struct = otherVar.theType.asInstanceOf[CStructure]
      struct.getFields.foreach{ field =>
        val baseField = TypeHelper.offsetof(struct, otherVar.address, field.getName, state)
        val theField = TypeHelper.offsetof(struct, dst.address, field.getName, state)
        theField.setValue(baseField.getValue.value)
      }
    } else {
      val casted = TypeHelper.cast(dst.theType, result.value).value
      dst.setValue(casted)
    }

    dst
  }

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

  def evaluatePointerArithmetic(left: RValue, right: RValue, operator: Int)(implicit state: State): RValue = {

    val isLeftPointer = TypeHelper.isPointer(left)
    val isRightPointer = TypeHelper.isPointer(right)

    if (operator == `op_minus` || operator == `op_plus`) {

      val baseType = if (left.isInstanceOf[Address]) {
        left.asInstanceOf[Address].baseType
      } else {
        TypeHelper.resolve(left.theType)
      }

      val rightBaseType = if (right.isInstanceOf[Address]) {
        right.asInstanceOf[Address].baseType
      } else {
        TypeHelper.resolve(right.theType)
      }

      val ptrSize = TypeHelper.sizeof(baseType)
      val rightPtrSize = TypeHelper.sizeof(rightBaseType)

      if (isLeftPointer && isRightPointer) {
        val result = if (operator == `op_minus`) {
          (left.value.asInstanceOf[Int] - right.value.asInstanceOf[Int]) / ptrSize
        } else {
          (left.value.asInstanceOf[Int] + right.value.asInstanceOf[Int]) / ptrSize
        }

        if (left.isInstanceOf[Address]) {
          Address(result, baseType)
        } else {
          new RValue(result, left.theType) {}
        }
      } else if (!isLeftPointer && isRightPointer) {
        val result = if (operator == `op_minus`) {
          left.value.asInstanceOf[Int] * rightPtrSize - right.value.asInstanceOf[Int]
        } else {
          left.value.asInstanceOf[Int] * rightPtrSize + right.value.asInstanceOf[Int]
        }

        if (right.isInstanceOf[Address]) {
          Address(result, rightBaseType)
        } else {
          new RValue(result, right.theType) {}
        }
      } else if (isLeftPointer && !isRightPointer) {
        val result = if (operator == `op_minus`) {
          left.value.asInstanceOf[Int] - right.value.asInstanceOf[Int] * ptrSize
        } else {
          left.value.asInstanceOf[Int] + right.value.asInstanceOf[Int] * ptrSize
        }

        if (left.isInstanceOf[Address]) {
          Address(result, baseType)
        } else {
          new RValue(result, left.theType) {}
        }
      } else {
        new RValue(calculate(left.value, right.value, operator), left.theType) {}
      }
    } else {
      new RValue(calculate(left.value, right.value, operator), left.theType) {}
    }
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

          case (x: Double, y: Int) => x + y
          case (x: Double, y: Double) => x + y
          case (x: Double, y: Float) => x + y

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
          case (x: Double, y: Int) => if (x % y >= 0) x % y else (x % y) + y
          case (x: Int, y: Double) => if (x % y >= 0) x % y else (x % y) + y
          case (x: Double, y: Double) => if (x % y >= 0) x % y else (x % y) + y
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

  def evaluate(node: IASTNode, x: ValueType, y: ValueType, operator: Int)(implicit state: State): RValue = {
    val isLeftPointer = TypeHelper.isPointer(x)
    val isRightPointer = TypeHelper.isPointer(y)

    val left = TypeHelper.resolve(x)
    val right = TypeHelper.resolve(y)

    if (isLeftPointer || isRightPointer) {
      evaluatePointerArithmetic(left, right, operator)
    } else {
      val result = calculate(left.value,  right.value, operator)

      // offical conversion rules
      val resultType = (left.theType, right.theType) match {
        case (l: IBasicType, r: IBasicType) => (l.getKind, r.getKind) match {
          case (`eDouble`, _) if (l.isLong) => new CBasicType(eDouble, IBasicType.IS_LONG)
          case (_, `eDouble`) if (r.isLong) => new CBasicType(eDouble, IBasicType.IS_LONG)
          case (`eDouble`, _) => new CBasicType(eDouble, 0)
          case (_, `eDouble`) => new CBasicType(eDouble, 0)
          case (`eFloat`, _) => new CBasicType(eFloat, 0)
          case (_, `eFloat`) => new CBasicType(eFloat, 0)
          case (_, _) if (l.isLong && l.isUnsigned) || (r.isLong && r.isUnsigned) => new CBasicType(eInt, IBasicType.IS_UNSIGNED | IBasicType.IS_LONG)
          case (_, _) if (l.isLong || r.isLong) => new CBasicType(eInt, IBasicType.IS_LONG)
          case (_, _) if (l.isUnsigned || r.isUnsigned) => new CBasicType(eInt, IBasicType.IS_UNSIGNED)
          case _ => new CBasicType(eInt, 0)
        }
        case _ => left.theType
      }

      new RValue(result, resultType) {}
    }
  }
}