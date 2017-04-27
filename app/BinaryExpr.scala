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
      case info @ AddressInfo(_, _) => info
    }
    
    val resolvedop2 = op2 match {
      case StringLiteral(str) =>
          state.createStringVariable(str, false)
      case x => TypeHelper.resolve(x)
    }
    
    val theVal = ValueInfo(dst.value, dst.theType)
    
    val result = performBinaryOperation(theVal, resolvedop2, op)
    
    val casted = TypeHelper.downcast(dst.theType, result).value
    state.setValue(casted, dst.address)
    
    dst
  }
  
  def performBinaryOperation(left: ValueInfo, right: ValueInfo, operator: Int)(implicit state: State): AnyVal = {
    
    val op1 = left.value
    val op2 = right.value
    
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
        TypeHelper.resolveBoolean(op1) && TypeHelper.resolveBoolean(op2)
      case `op_logicalOr` =>
        TypeHelper.resolveBoolean(op1) || TypeHelper.resolveBoolean(op2)
      case `op_assign` =>
        op2
      case _ => throw new Exception("unhandled binary operator: " + operator); 0
    }

    result
  }
  
  def evaluate(left: ValueInfo, right: ValueInfo, operator: Int)(implicit state: State): ValueInfo = {

    val op1 = left

    val op2 =
      operator match {
      case `op_plus` | `op_minus` =>
          if (TypeHelper.isPointer(left.theType)) {
            // pointers get special treatment in binary expressions sometimes
            ValueInfo(right.value.asInstanceOf[Int] * TypeHelper.sizeof(TypeHelper.resolve(left.theType)), TypeHelper.pointerType)
          } else {
            right
          }
      case _ => right
    }

    ValueInfo(performBinaryOperation(op1, op2, operator), left.theType)
  }
  
  def parse(binaryExpr: IASTBinaryExpression)(implicit state: State): Any = {

    val op2 = TypeHelper.resolve(state.stack.pop)
    
    val rawOp1 = state.stack.pop

    rawOp1 match {
      case info @ AddressInfo(_, theType)  =>
        val value = state.readVal(info.address, info.theType)
        val result = evaluate(value, op2, binaryExpr.getOperator)

        if (result.value.isInstanceOf[Boolean]) {
          result.value
        } else if (TypeHelper.resolve(value.theType).isUnsigned) {
          ValueInfo(result.value, theType)
        } else if (TypeHelper.isPointer(info.theType)) {
          ValueInfo(Address(result.value.asInstanceOf[Int]), info.theType)
        } else {
          result.value
        }
      case _ =>
        val simple = TypeHelper.resolve(rawOp1)
        performBinaryOperation(simple, op2, binaryExpr.getOperator)
    }


  }
}