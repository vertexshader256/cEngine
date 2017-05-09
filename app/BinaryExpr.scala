package app.astViewer

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._
import org.eclipse.cdt.core.dom.ast._

object BinaryExpr {
  
  def parseAssign(op: Int, op1: Stackable, op2: Stackable)(implicit state: State): AddressInfo = {

    val dst: AddressInfo = op1 match {
      case info @ AddressInfo(_, _) => info
    }
    
    val resolvedop2 = op2 match {
      case StringLiteral(str) =>
          state.createStringVariable(str, false)
      case addr @ AddressInfo(_, _) => addr.value
      case value @ ValueInfo(_, _) => value
    }
    
    val theVal = dst.value
    
    val result = evaluate(theVal, resolvedop2, op)
    
    val casted = TypeHelper.cast(dst.theType, result.value).value
    state.setValue(casted, dst.address)
    
    dst
  }
  
  def evaluate(left: ValueInfo, right: ValueInfo, operator: Int)(implicit state: State): ValueInfo = {
    
    val op1 = left.value
    val op2 = if (left.theType.isInstanceOf[IPointerType] && (operator == `op_minus` || operator == `op_plus`)) {
      // pointers get special treatment in binary expressions sometimes
      right.value.asInstanceOf[Int] * TypeHelper.sizeof(TypeHelper.resolve(left.theType))
    } else {
      right.value
    }
    
    val result: AnyVal = operator match {
      case `op_multiply` | `op_multiplyAssign` =>
        (op1, op2) match {
          case (x: Int, y: Character) => x * y
          case (x: Int, y: Short) => x * y
          case (x: Int, y: Int) => x * y
          case (x: Int, y: Float) => x * y
          case (x: Int, y: Double) => x * y
          case (x: Int, y: Long) => x * y
          
          case (x: Character, y: Character) => x * y
          case (x: Character, y: Short) => x * y
          case (x: Character, y: Int) => x * y
          case (x: Character, y: Float) => x * y
          case (x: Character, y: Long) => x * y
          case (x: Character, y: Double) => x * y
          
          case (x: Float, y: Character) => x * y
          case (x: Float, y: Short) => x * y
          case (x: Float, y: Int) => x * y
          case (x: Float, y: Double) => x * y
          case (x: Float, y: Float) => x * y
          case (x: Float, y: Long) => x * y
          
          case (x: Double, y: Character) => x * y
          case (x: Double, y: Short) => x * y          
          case (x: Double, y: Int) => x * y
          case (x: Double, y: Double) => x * y
          case (x: Double, y: Float) => x * y
          case (x: Double, y: Long) => x * y
          
          case (x: Long, y: Character) => x * y
          case (x: Long, y: Short) => x * y          
          case (x: Long, y: Int) => x * y
          case (x: Long, y: Float) => x * y
          case (x: Long, y: Double) => x * y
          case (x: Long, y: Long) => x * y
          
          case (x: Short, y: Character) => x * y         
          case (x: Short, y: Short) => x * y
          case (x: Short, y: Int) => x * y
          case (x: Short, y: Float) => x * y
          case (x: Short, y: Double) => x * y
          case (x: Short, y: Long) => x * y
        }
      case `op_plus` | `op_plusAssign` =>
        (op1, op2) match {
          case (x: Int, y: Character) => x + y
          case (x: Int, y: Int) => x + y
          case (x: Int, y: Short) => x + y
          case (x: Int, y: Float) => x + y
          case (x: Int, y: Double) => x + y
          case (x: Int, y: Long) => x + y
          
          case (x: Character, y: Character) => x + y  
          case (x: Character, y: Short) => x + y          
          case (x: Character, y: Int) => x + y
          case (x: Character, y: Float) => x + y
          case (x: Character, y: Double) => x + y
          case (x: Character, y: Long) => x + y
          
          case (x: Float, y: Character) => x + y
          case (x: Float, y: Short) => x + y
          case (x: Float, y: Int) => x + y
          case (x: Float, y: Float) => x + y
          case (x: Float, y: Double) => x + y
          case (x: Float, y: Long) => x + y
          
          case (x: Double, y: Character) => x + y
          case (x: Double, y: Short) => x + y          
          case (x: Double, y: Int) => x + y
          case (x: Double, y: Double) => x + y
          case (x: Double, y: Float) => x + y
          case (x: Double, y: Long) => x + y
          
          case (x: Long, y: Character) => x + y
          case (x: Long, y: Short) => x + y
          case (x: Long, y: Int) => x + y
          case (x: Long, y: Float) => x + y
          case (x: Long, y: Double) => x + y
          case (x: Long, y: Long) => x + y
          
          case (x: Short, y: Character) => x + y
          case (x: Short, y: Short) => x + y
          case (x: Short, y: Int) => x + y
          case (x: Short, y: Float) => x + y
          case (x: Short, y: Double) => x + y
          case (x: Short, y: Long) => x + y
          
        }
      case `op_minus` | `op_minusAssign` =>
        (op1, op2) match {
          case (x: Int, y: Character) => x - y
          case (x: Int, y: Short) => x - y
          case (x: Int, y: Int) => x - y
          case (x: Int, y: Float) => x - y
          case (x: Int, y: Double) => x - y
          case (x: Int, y: Long) => x - y
          
          case (x: Character, y: Character) => x - y
          case (x: Character, y: Short) => x - y         
          case (x: Character, y: Int) => x - y
          case (x: Character, y: Float) => x - y
          case (x: Character, y: Double) => x - y
          case (x: Character, y: Long) => x - y
          
          case (x: Float, y: Character) => x - y
          case (x: Float, y: Short) => x - y
          case (x: Float, y: Int) => x - y
          case (x: Float, y: Double) => x - y
          case (x: Float, y: Float) => x - y
          case (x: Float, y: Long) => x - y
          
          case (x: Double, y: Character) => x - y
          case (x: Double, y: Short) => x - y
          case (x: Double, y: Int) => x - y
          case (x: Double, y: Double) => x - y
          case (x: Double, y: Float) => x - y
          case (x: Double, y: Long) => x - y
          
          case (x: Long, y: Character) => x - y
          case (x: Long, y: Short) => x - y         
          case (x: Long, y: Int) => x - y
          case (x: Long, y: Float) => x - y
          case (x: Long, y: Double) => x - y
          case (x: Long, y: Long) => x - y
          
          case (x: Short, y: Character) => x - y
          case (x: Short, y: Short) => x - y
          case (x: Short, y: Int) => x - y
          case (x: Short, y: Float) => x - y
          case (x: Short, y: Double) => x - y
          case (x: Short, y: Long) => x - y
        }
      case `op_divide` =>
        val result: Double = (op1, op2) match {
          case (x: Int, y: Character) => x / y
          case (x: Int, y: Short) => x / y
          case (x: Int, y: Int) => x / y
          case (x: Int, y: Float) => x / y
          case (x: Int, y: Double) => x / y
          case (x: Int, y: Long) => x / y
          
          case (x: Character, y: Character) => x / y
          case (x: Character, y: Short) => x / y
          case (x: Character, y: Int) => x / y
          case (x: Character, y: Float) => x / y
          case (x: Character, y: Double) => x / y
          case (x: Character, y: Long) => x / y

          case (x: Float, y: Character) => x / y
          case (x: Float, y: Short) => x / y
          case (x: Float, y: Int) => x / y
          case (x: Float, y: Double) => x / y
          case (x: Float, y: Float) => x / y
          case (x: Float, y: Long) => x / y

          case (x: Double, y: Int) => x / y
          case (x: Double, y: Double) => x / y
          case (x: Double, y: Char) => x / y
          case (x: Double, y: Float) => x / y
          case (x: Double, y: Long) => x / y
          case (x: Double, y: Short) => x / y
 
          case (x: Long, y: Character) => x / y
          case (x: Long, y: Short) => x / y
          case (x: Long, y: Int) => x / y
          case (x: Long, y: Float) => x / y
          case (x: Long, y: Double) => x / y
          case (x: Long, y: Long) => x / y

          case (x: Short, y: Character) => x / y
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
        }
      case `op_shiftLeft` | `op_shiftLeftAssign` =>
        (op1, op2) match {
          case (x: Long, y: Int) => x << y
          case (x: Int, y: Int) => x << y
        }
      case `op_equals` =>
        (op1, op2) match {
          case (x: Character, y: Int) => x == y
          case (x: Int, y: Int) => x == y
          case (x: Character, y: Character) => x == y
          case (x: Short, y: Short) => x == y
          case (x: Long, y: Int) => x == y
          case (x: Int, y: Long) => x == y
          case (x: Int, y: Character) => x == y
          case (x: Double, y: Int) => x == y
          case (x: Int, y: Double) => x == y
          case (x: Double, y: Double) => x == y
          case (x: Long, y: Long) => x == y
        }
      case `op_notequals` =>
        !evaluate(left, right, op_equals).value.asInstanceOf[Boolean]
      case `op_greaterThan` =>
        (op1, op2) match {
          case (x: Long, y: Long) => x > y
          case (x: Int, y: Long) => x > y
          case (x: Long, y: Int) => x > y
          case (x: Int, y: Int) => x > y
          case (x: Double, y: Int) => x > y
          case (x: Int, y: Double) => x > y
          case (x: Double, y: Double) => x > y
        }
      case `op_greaterEqual` =>
        (op1, op2) match {
          case (x: Int, y: Long) => x >= y
          case (x: Long, y: Int) => x >= y
          case (x: Long, y: Long) => x >= y
          case (x: Int, y: Int) => x >= y
          case (x: Character, y: Character) => x >= y
          case (x: Character, y: Int) => x >= y
          case (x: Int, y: Character) => x >= y
          case (x: Double, y: Int) => x >= y
          case (x: Int, y: Double) => x >= y
          case (x: Double, y: Double) => x >= y
        }  
      case `op_lessThan` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x < y
          case (x: Int, y: Short) => x < y
          case (x: Int, y: Character) => x < y
          case (x: Int, y: Long) => x < y
          case (x: Int, y: Double) => x < y
          
          case (x: Short, y: Int) => x < y
          
          case (x: Character, y: Int) => x < y
          case (x: Long, y: Int) => x < y
          case (x: Long, y: Long) => x < y
          case (x: Double, y: Int) => x < y         
          case (x: Double, y: Double) => x < y
          case (x: Float, y: Double) => x < y
          case (x: Float, y: Float) => x < y
        }
      case `op_lessEqual` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x <= y
          case (x: Character, y: Int) => x <= y
          case (x: Int, y: Character) => x <= y
          case (x: Character, y: Character) => x <= y
          case (x: Double, y: Int) => x <= y
          case (x: Int, y: Double) => x <= y
          case (x: Double, y: Double) => x <= y
        }  
      case `op_modulo` =>
        (op1, op2) match {
          case (x: Long, y: Long) => x % y
          case (x: Int, y: Int) => x % y
          case (x: Int, y: Long) => x % y
          case (x: Long, y: Int) => x % y
          case (x: Double, y: Int) => x % y
          case (x: Int, y: Double) => x % y
          case (x: Double, y: Double) => x % y
        } 
      case `op_binaryOr`  | `op_binaryOrAssign`=>
        (op1, op2) match {
          case (x: Int, y: Int) => x | y
        }  
      case `op_binaryXor` | `op_binaryXorAssign` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x ^ y
          case (x: Character, y: Int) => x ^ y
          case (x: Int, y: Char) => x ^ y
        }   
      case `op_binaryAnd` | `op_binaryAndAssign` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x & y
          case (x: Character, y: Int) => x & y
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

    ValueInfo(result, TypeHelper.getType(result))
  }
}