package scala.c_engine

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._
import org.eclipse.cdt.core.dom.ast._

object BinaryExpr {
  
  def parseAssign(op: Int, op1: ValueType, op2: ValueType)(implicit state: State): LValue = {

    val dst: LValue = op1 match {
      case info @ LValue(_, _) => info
    }
    
    val resolvedop2 = op2 match {
      case StringLiteral(str) =>
          state.createStringVariable(str, false)
      case addr @ LValue(_, _) => addr.value
      case value @ RValue(_, _) => value
    }
    
    val theVal = dst.value

    val result = evaluate(theVal, resolvedop2, op)
    
    val casted = TypeHelper.cast(dst.theType, result.value).value
    state.setValue(casted, dst.address)
    
    dst
  }
  
  def evaluate(left: RValue, right: RValue, operator: Int)(implicit state: State): RValue = {
    
    val op1 = left.value
    val op2 = if ((left.theType.isInstanceOf[IPointerType] || left.theType.isInstanceOf[IArrayType]) && (operator == `op_minus` || operator == `op_plus`)) {
      // increment by the size of the left arg
      right.value.asInstanceOf[Int] * TypeHelper.sizeof(TypeHelper.resolve(left.theType))
    } else {
      right.value
    }
    
    val result: AnyVal = operator match {
      case `op_multiply` | `op_multiplyAssign` =>
        (op1, op2) match {
          case (x: int, y: char) => x * y
          case (x: int, y: short) => x * y
          case (x: int, y: int) => x * y
          case (x: int, y: float) => x * y
          case (x: int, y: double) => x * y
          case (x: int, y: Long) => x * y
          
          case (x: char, y: char) => x * y
          case (x: char, y: Short) => x * y
          case (x: char, y: Int) => x * y
          case (x: char, y: Float) => x * y
          case (x: char, y: Double) => x * y
          case (x: char, y: Long) => x * y
          
          case (x: Float, y: char) => x * y
          case (x: Float, y: Short) => x * y
          case (x: Float, y: Int) => x * y
          case (x: Float, y: Double) => x * y
          case (x: Float, y: Float) => x * y
          
          case (x: Double, y: char) => x * y
          case (x: Double, y: Short) => x * y          
          case (x: Double, y: Int) => x * y
          case (x: Double, y: Double) => x * y
          case (x: Double, y: Float) => x * y
          
          case (x: Long, y: char) => x * y
          case (x: Long, y: Short) => x * y          
          case (x: Long, y: Int) => x * y
          case (x: Long, y: Float) => x * y
          case (x: Long, y: Double) => x * y
          case (x: Long, y: Long) => x * y
          
          case (x: Short, y: char) => x * y
          case (x: Short, y: Short) => x * y
          case (x: Short, y: Int) => x * y
          case (x: Short, y: Float) => x * y
          case (x: Short, y: Double) => x * y
          case (x: Short, y: Long) => x * y
        }
      case `op_plus` | `op_plusAssign` =>
        (op1, op2) match {
          case (x: Int, y: char) => x + y
          case (x: Int, y: Int) => x + y
          case (x: Int, y: Short) => x + y
          case (x: Int, y: Float) => x + y
          case (x: Int, y: Double) => x + y
          case (x: Int, y: Long) => x + y
          
          case (x: char, y: char) => x + y
          case (x: char, y: Short) => x + y
          case (x: char, y: Int) => x + y
          case (x: char, y: Float) => x + y
          case (x: char, y: Double) => x + y
          case (x: char, y: Long) => x + y
          
          case (x: Float, y: char) => x + y
          case (x: Float, y: Short) => x + y
          case (x: Float, y: Int) => x + y
          case (x: Float, y: Float) => x + y
          case (x: Float, y: Double) => x + y
          
          case (x: Double, y: char) => x + y
          case (x: Double, y: Short) => x + y          
          case (x: Double, y: Int) => x + y
          case (x: Double, y: Double) => x + y
          case (x: Double, y: Float) => x + y
          
          case (x: Long, y: char) => x + y
          case (x: Long, y: Short) => x + y
          case (x: Long, y: Int) => x + y
          case (x: Long, y: Float) => x + y
          case (x: Long, y: Double) => x + y
          case (x: Long, y: Long) => x + y
          
          case (x: Short, y: char) => x + y
          case (x: Short, y: Short) => x + y
          case (x: Short, y: Int) => x + y
          case (x: Short, y: Float) => x + y
          case (x: Short, y: Double) => x + y
          case (x: Short, y: Long) => x + y
        }
      case `op_minus` | `op_minusAssign` =>
        (op1, op2) match {
          case (x: Int, y: char) => x - y
          case (x: Int, y: Short) => x - y
          case (x: Int, y: Int) => x - y
          case (x: Int, y: Float) => x - y
          case (x: Int, y: Double) => x - y
          case (x: Int, y: Long) => x - y
          
          case (x: char, y: char) => x - y
          case (x: char, y: Short) => x - y
          case (x: char, y: Int) => x - y
          case (x: char, y: Float) => x - y
          case (x: char, y: Double) => x - y
          case (x: char, y: Long) => x - y
          
          case (x: Float, y: char) => x - y
          case (x: Float, y: Short) => x - y
          case (x: Float, y: Int) => x - y
          case (x: Float, y: Double) => x - y
          case (x: Float, y: Float) => x - y
          
          case (x: Double, y: char) => x - y
          case (x: Double, y: Short) => x - y
          case (x: Double, y: Int) => x - y
          case (x: Double, y: Double) => x - y
          case (x: Double, y: Float) => x - y
          
          case (x: Long, y: char) => x - y
          case (x: Long, y: Short) => x - y         
          case (x: Long, y: Int) => x - y
          case (x: Long, y: Float) => x - y
          case (x: Long, y: Double) => x - y
          case (x: Long, y: Long) => x - y
          
          case (x: Short, y: char) => x - y
          case (x: Short, y: Short) => x - y
          case (x: Short, y: Int) => x - y
          case (x: Short, y: Float) => x - y
          case (x: Short, y: Double) => x - y
          case (x: Short, y: Long) => x - y
        }
      case `op_divide` | `op_divideAssign` =>
        val result: Double = (op1, op2) match {
          case (x: Int, y: char) => x / y
          case (x: Int, y: Short) => x / y
          case (x: Int, y: Int) => x / y
          case (x: Int, y: Float) => x / y
          case (x: Int, y: Double) => x / y
          case (x: Int, y: Long) => x / y
          
          case (x: char, y: char) => x / y
          case (x: char, y: Short) => x / y
          case (x: char, y: Int) => x / y
          case (x: char, y: Float) => x / y
          case (x: char, y: Double) => x / y
          case (x: char, y: Long) => x / y

          case (x: Float, y: char) => x / y
          case (x: Float, y: Short) => x / y
          case (x: Float, y: Int) => x / y
          case (x: Float, y: Double) => x / y
          case (x: Float, y: Float) => x / y

          case (x: Double, y: Int) => x / y
          case (x: Double, y: Double) => x / y
          case (x: Double, y: Char) => x / y
          case (x: Double, y: Float) => x / y
          case (x: Double, y: Short) => x / y
 
          case (x: Long, y: char) => x / y
          case (x: Long, y: Short) => x / y
          case (x: Long, y: Int) => x / y
          case (x: Long, y: Float) => x / y
          case (x: Long, y: Double) => x / y
          case (x: Long, y: Long) => x / y

          case (x: Short, y: char) => x / y
          case (x: Short, y: Short) => x / y
          case (x: Short, y: Int) => x / y
          case (x: Short, y: Float) => x / y
          case (x: Short, y: Double) => x / y
          case (x: Short, y: Long) => x / y
        }
        result
      case `op_shiftRight` | `op_shiftRightAssign` =>
        (op1, op2) match {
          case (x: Long, y: Int) => x >> y
          case (x: Int, y: Int) => x >> y
          case (x: Short, y: Int) => x >> y
          case (x: Int, y: Short) => x >> y
          case (x: Int, y: char) => x >> y
          case (x: char, y: Int) => x >> y

        }
      case `op_shiftLeft` | `op_shiftLeftAssign` =>
        (op1, op2) match {
          case (x: Long, y: Int) => x << y
          case (x: Int, y: Int) => x << y
          case (x: Short, y: Int) => x << y
          case (x: Int, y: Short) => x << y
          case (x: char, y: Int) => x << y
          case (x: Int, y: char) => x << y
        }
      case `op_equals` =>
        op1 == op2
      case `op_notequals` =>
        !evaluate(left, right, op_equals).value.asInstanceOf[Boolean]
      case `op_greaterThan` =>
        (op1, op2) match {
          case (x: Int, y: char) => x > y
          case (x: Int, y: Short) => x > y
          case (x: Int, y: Int) => x > y
          case (x: Int, y: Float) => x > y
          case (x: Int, y: Double) => x > y
          case (x: Int, y: Long) => x > y

          case (x: char, y: char) => x > y
          case (x: char, y: Short) => x > y
          case (x: char, y: Int) => x > y
          case (x: char, y: Float) => x > y
          case (x: char, y: Double) => x > y
          case (x: char, y: Long) => x > y

          case (x: Float, y: char) => x > y
          case (x: Float, y: Short) => x > y
          case (x: Float, y: Int) => x > y
          case (x: Float, y: Double) => x > y
          case (x: Float, y: Float) => x > y

          case (x: Double, y: Int) => x > y
          case (x: Double, y: Double) => x > y
          case (x: Double, y: Char) => x > y
          case (x: Double, y: Float) => x > y
          case (x: Double, y: Short) => x > y

          case (x: Long, y: char) => x > y
          case (x: Long, y: Short) => x > y
          case (x: Long, y: Int) => x > y
          case (x: Long, y: Float) => x > y
          case (x: Long, y: Double) => x > y
          case (x: Long, y: Long) => x > y

          case (x: Short, y: char) => x > y
          case (x: Short, y: Short) => x > y
          case (x: Short, y: Int) => x > y
          case (x: Short, y: Float) => x > y
          case (x: Short, y: Double) => x > y
          case (x: Short, y: Long) => x > y
        }
      case `op_greaterEqual` =>
        evaluate(left, right, op_greaterThan).value.asInstanceOf[Boolean] || evaluate(left, right, op_equals).value.asInstanceOf[Boolean]
      case `op_lessThan` =>
        !evaluate(left, right, op_greaterEqual).value.asInstanceOf[Boolean]
      case `op_lessEqual` =>
        !evaluate(left, right, op_greaterThan).value.asInstanceOf[Boolean]
      case `op_modulo` =>

        (op1, op2) match {
          case (x: Int, y: Short) => if (x % y >= 0) x % y else (x % y) + y
          case (x: Long, y: Long) => if (x % y >= 0) x % y else (x % y) + y
          case (x: Long, y: Int) =>
            println("-1----------")
            if (x % y >= 0) x % y else (x % y) + y
          case (x: Int, y: Int) =>
            println("-----------")
            if (x % y >= 0) x % y else (x % y) + y
          case (x: Double, y: Int) => if (x % y >= 0) x % y else (x % y) + y
          case (x: Int, y: Double) => if (x % y >= 0) x % y else (x % y) + y
          case (x: Double, y: Double) => if (x % y >= 0) x % y else (x % y) + y
        } 
      case `op_binaryOr`  | `op_binaryOrAssign`=>
        (op1, op2) match {
          case (x: Int, y: Int) => x | y
        }  
      case `op_binaryXor` | `op_binaryXorAssign` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x ^ y
          case (x: Int, y: char) => x ^ y
          case (x: Int, y: Short) => x ^ y
          case (x: char, y: Int) => x ^ y
          case (x: Int, y: Char) => x ^ y
        }   
      case `op_binaryAnd` | `op_binaryAndAssign` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x & y
          case (x: char, y: Int) => x & y
          case (x: Int, y: Char) => x & y
          case (x: Int, y: Long) => x & y
          
          case (x: Long, y: Long) => x & y
          case (x: Long, y: Int) => x & y
        }
      case `op_logicalAnd` =>
        TypeHelper.resolveBoolean(op1) && TypeHelper.resolveBoolean(op2)
      case `op_logicalOr` =>
        TypeHelper.resolveBoolean(op1) || TypeHelper.resolveBoolean(op2)
      case `op_assign` =>
        op2
      case _ => throw new Exception("unhandled binary operator: " + operator); 0
    }

    RValue(result, left.theType)
  }
}