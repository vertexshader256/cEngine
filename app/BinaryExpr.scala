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
  
  def parseAssign(node: IASTNode, op: Int, op1: Any, op2: Any)(implicit state: State): Any = {

    val dst: AddressInfo = op1 match {
      case Variable(info: Variable[_]) => info.info
      case info @ AddressInfo(_, _) => info
      case ValueInfo(value, info) => AddressInfo(Address(value.asInstanceOf[Byte].toInt), info)
    }

    if (!op2.isInstanceOf[IASTFunctionDefinition]) {
      val resolvedop2 = TypeHelper.resolve(op2)
      
      val theVal = ValueInfo(state.readVal(dst.address, dst.theType).value, dst.theType)
      
      val result = op match {
        case `op_plusAssign` =>
          performBinaryOperation(theVal.value, resolvedop2, op_plus)
        case `op_minusAssign` =>
          performBinaryOperation(theVal.value, resolvedop2, op_minus)
        case `op_multiplyAssign` =>
          performBinaryOperation(theVal.value, resolvedop2, op_multiply)
        case `op_binaryXorAssign` =>
          performBinaryOperation(theVal.value, resolvedop2, op_binaryXor)
        case `op_shiftRightAssign` =>
          performBinaryOperation(theVal.value, resolvedop2, op_shiftRight)
        case `op_assign` =>
          resolvedop2
      }  
      
      val casted = TypeHelper.downcast(dst.theType, result).value
      state.setValue(casted, dst.address)
      
      resolvedop2
    } else {
      op1 match {
        case Variable(theVar: Variable[_]) => theVar.node = op2.asInstanceOf[IASTFunctionDefinition]
      }
    }
  }
  
  def performTypedBinaryOperation(left: ValueInfo, right: AnyVal, operator: Int): Option[AnyVal] = {
    
    val op1 = left.value
    val op2 = right
    
    val result: Option[AnyVal] = operator match {
      case `op_plus` =>
        (op1, op2) match {
          case (x: Int, y: Int) if (TypeHelper.isPointer(left.theType)) => Some(x + y * TypeHelper.sizeof(TypeHelper.resolve(left.theType)))
          case _ => None
        }
      case `op_minus` =>
        (op1, op2) match {
          case (x: Int, y: Int) if (TypeHelper.isPointer(left.theType)) => Some(x - y * TypeHelper.sizeof(TypeHelper.resolve(left.theType)))
          case _ => None
        }
      case _ => None
    }

    result
  }
  
  def performBinaryOperation(left: AnyVal, right: AnyVal, operator: Int): AnyVal = {
    
    val op1 = left
    val op2 = right
    
    val result: AnyVal = operator match {
      case `op_multiply` =>
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
      case `op_plus` =>
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
      case `op_minus` =>
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
      case `op_shiftRight` =>
        (op1, op2) match {
          case (x: Long, y: Int) => x >> y
          case (x: Int, y: Int) => x >> y
        }
      case `op_shiftLeft` =>
        (op1, op2) match {
          case (x: Long, y: Int) => x << y
          case (x: Int, y: Int) => x << y
        }
      case `op_equals` =>
        (op1, op2) match {
          case (x: Character, y: Int) => x == y
          case (x: Int, y: Int) => x == y
          case (x: Character, y: Character) => x == y
          case (x: Double, y: Int) => x == y
          case (x: Int, y: Double) => x == y
          case (x: Double, y: Double) => x == y
        }
      case `op_notequals` =>
        !performBinaryOperation(left, right, op_equals).asInstanceOf[Boolean]
      case `op_greaterThan` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x > y
          case (x: Double, y: Int) => x > y
          case (x: Int, y: Double) => x > y
          case (x: Double, y: Double) => x > y
        }
      case `op_greaterEqual` =>
        (op1, op2) match {
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
          case (x: Short, y: Int) => x < y
          case (x: Character, y: Int) => x < y
          case (x: Long, y: Int) => x < y
          case (x: Int, y: Short) => x < y
          case (x: Int, y: Character) => x < y
          case (x: Int, y: Long) => x < y
          case (x: Double, y: Int) => x < y
          case (x: Int, y: Double) => x < y
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
          case (x: Int, y: Int) => x % y
          case (x: Double, y: Int) => x % y
          case (x: Int, y: Double) => x % y
          case (x: Double, y: Double) => x % y
        } 
      case `op_binaryOr` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x | y
        }  
      case `op_binaryXor` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x ^ y
          case (x: Character, y: Int) => x ^ y
          case (x: Int, y: Char) => x ^ y
        }   
      case `op_binaryAnd` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x & y
          case (x: Character, y: Int) => x & y
          case (x: Int, y: Char) => x & y
        }
      case `op_logicalAnd` =>
        (op1, op2) match {
          case (x: Boolean, y: Boolean) => x && y
          case (x: Boolean, y: Int) => x && (y > 0)
          case (x: Boolean, y: Character) => x && (y > 0)
          case (x: Int, y: Boolean) => (x > 0) && y
          case (x: Character, y: Boolean) => (x > 0) && y
        }
      case `op_logicalOr` =>
        (op1, op2) match {
          case (x: Boolean, y: Boolean) => x || y
          case (x: Int, y: Boolean) => (x > 0) || y
        }
      case _ => throw new Exception("unhandled binary operator: " + operator); 0
    }

    result
  }
  
  def evaluate(op1: ValueInfo, op2: AnyVal, operator: Int) = {
    performTypedBinaryOperation(op1, op2, operator).getOrElse{
      performBinaryOperation(op1.value, op2, operator)
    }
  }
  
  def parse(binaryExpr: IASTBinaryExpression)(implicit state: State): Any = {

    val op2 = TypeHelper.resolve(state.stack.pop)
    
    val rawOp1 = state.stack.pop
    
    rawOp1 match {
      case Variable(info: Variable[_])  =>
        val result = performTypedBinaryOperation(info.value, op2, binaryExpr.getOperator).getOrElse{
          performBinaryOperation(info.value.value, op2, binaryExpr.getOperator)
        }
        
        if (!result.isInstanceOf[Boolean] && TypeHelper.resolve(info.value.theType).isUnsigned) {
          TypeHelper.cast(TypeHelper.resolve(info.value.theType), result).value
        } else if (!result.isInstanceOf[Boolean] && TypeHelper.isPointer(info.info.theType)) {
          ValueInfo(Address(result.asInstanceOf[Int]), info.info.theType)
        } else {
          result
        }
      case _ =>
        val simple = TypeHelper.resolve(rawOp1)
        performBinaryOperation(simple, op2, binaryExpr.getOperator)
    }
  }
}