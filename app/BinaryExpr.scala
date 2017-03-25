package app.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType
import org.eclipse.cdt.internal.core.dom.parser.c.CStructure

object BinaryExpr {
  
  def parseAssign(op: Int, op1: Any, op2: Any)(implicit state: State): AddressInfo = {

    val dst: AddressInfo = op1 match {
      case Variable(info: Variable) => info.info
      case info @ AddressInfo(_, _) => info
    }
    
    val resolvedop2 = op2 match {
      case StringLiteral(str) =>
          state.createStringVariable(str, false)
      case x => TypeHelper.resolve(x).value
    }
    
    val theVal = ValueInfo(state.readVal(dst.address, dst.theType).value, dst.theType)
    
    val result = performBinaryOperation(theVal.value, resolvedop2, op)
    
    val casted = TypeHelper.downcast(dst.theType, result).value
    state.setValue(casted, dst.address)
    
    dst
  }
  
  def performBinaryOperation(left: AnyVal, right: AnyVal, operator: Int): AnyVal = {
    
    val op1 = left
    val op2 = right
    
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
        !performBinaryOperation(left, right, op_equals).asInstanceOf[Boolean]
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
        (op1, op2) match {
          case (x: Boolean, y: Boolean) => x && y
          case (x: Boolean, y: Int) => x && (y > 0)
          case (x: Boolean, y: Long) => x && (y > 0)
          case (x: Boolean, y: Character) => x && (y > 0)
          case (x: Int, y: Boolean) => (x > 0) && y
          case (x: Int, y: Int) => (x > 0) && (y > 0)
          case (x: Long, y: Long) => (x > 0) && (y > 0)
          case (x: Short, y: Short) => (x > 0) && (y > 0)
          case (x: Character, y: Character) => (x > 0) && (y > 0)
          case (x: Int, y: Long) => (x > 0) && (y > 0)
          case (x: Long, y: Int) => (x > 0) && (y > 0)
          case (x: Int, y: Short) => (x > 0) && (y > 0)
          case (x: Short, y: Int) => (x > 0) && (y > 0)
          case (x: Int, y: Character) => (x > 0) && (y > 0)
          case (x: Character, y: Int) => (x > 0) && (y > 0)
          case (x: Character, y: Boolean) => (x > 0) && y
        }
      case `op_logicalOr` =>
        (op1, op2) match {
          case (x: Boolean, y: Boolean) => x || y
          case (x: Int, y: Boolean) => (x > 0) || y
          case (x: Int, y: Int) => (x > 0) || (y > 0)
        }
      case `op_assign` =>
        op2
      case _ => throw new Exception("unhandled binary operator: " + operator); 0
    }

    result
  }
  
  def evaluate(left: ValueInfo, right: AnyVal, operator: Int): AnyVal = {

    val op1 = left.value

    val op2: AnyVal =
      operator match {
      case `op_plus` | `op_minus` =>
          if (TypeHelper.isPointer(left.theType)) {
            // pointers get special treatment in binary expressions sometimes
            right.asInstanceOf[Int] * TypeHelper.sizeof(TypeHelper.resolve(left.theType))
          } else {
            right
          }
      case _ => right
    }

    performBinaryOperation(op1, op2, operator)
  }
  
  def parse(binaryExpr: IASTBinaryExpression)(implicit state: State): Any = {

    val op2 = TypeHelper.resolve(state.stack.pop).value
    
    val rawOp1 = state.stack.pop

    rawOp1 match {
      case Variable(info: Variable)  =>
        val result = evaluate(info.value, op2, binaryExpr.getOperator)

        if (!result.isInstanceOf[Boolean] && TypeHelper.resolve(info.value.theType).isUnsigned) {
          TypeHelper.cast(info.value.theType, result).value
        } else if (!result.isInstanceOf[Boolean] && TypeHelper.isPointer(info.info.theType)) {
          ValueInfo(Address(result.asInstanceOf[Int]), info.info.theType)
        } else {
          result
        }
      case _ =>
        val simple = TypeHelper.resolve(rawOp1).value
        performBinaryOperation(simple, op2, binaryExpr.getOperator)
    }
  }
}