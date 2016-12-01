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
  
  def parseAssign(node: IASTNode, op: Int, op1: Any, op2: Any, state: State): Any = {

    val dst: AddressInfo = op1 match {
      case VarRef(name) =>
        val variable = state.vars.resolveId(name)
        variable.info
      case info @ AddressInfo(_, _) => info
      case ValueInfo(value, info) => AddressInfo(Address(value.asInstanceOf[Byte].toInt), info)
    }

    val resolvedop2 = ValueInfo(op2 match {
      case lit @ Literal(_) => lit.cast.value
      case VarRef(name)  =>      
        val theVar = state.vars.resolveId(name)
        theVar.value.value
      case AddressInfo(addr, theType) => 
        state.readVal(addr, theType).value
      case Address(addy) => addy
      case ValueInfo(x, _) => x
      case long: Long => long
      case int: Int => int
      case doub: Double => doub
      case float: Float => float 
    }, null)
    
    val theVal = ValueInfo(state.readVal(dst.address, dst.theType).value, dst.theType)
    
    val result = op match {
      case `op_plusAssign` =>
        performBinaryOperation(theVal.value, resolvedop2.value, op_plus)
      case `op_minusAssign` =>
        performBinaryOperation(theVal.value, resolvedop2.value, op_minus)
      case `op_multiplyAssign` =>
        performBinaryOperation(theVal.value, resolvedop2.value, op_multiply)
      case `op_binaryXorAssign` =>
        performBinaryOperation(theVal.value, resolvedop2.value, op_binaryXor)
      case `op_shiftRightAssign` =>
        performBinaryOperation(theVal.value, resolvedop2.value, op_shiftRight)
      case `op_assign` =>
        resolvedop2.value
    }  
    
    val casted = TypeHelper.downcast(dst.theType, result).value
    state.setValue(casted, dst.address)
    
    resolvedop2
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
  
  def parse(binaryExpr: IASTBinaryExpression, state: State): Any = {
   
    def resolve(op: Any, context: State): AnyVal = {
      op match {
        case lit @ Literal(_) => lit.cast.value
        case prim @ ValueInfo(_, _) => prim.value
        case VarRef(name)  =>      
          val theVar = context.vars.resolveId(name)
          theVar.value.value
        case AddressInfo(addy, theType) =>
          context.readVal(addy, TypeHelper.resolve(theType)).value
        case int: Int => int
        case float: Float => float
        case double: Double => double
        case short: Short => short
        case char: Character => char
        case boolean: Boolean => boolean
        case long: Long => long
      }
    }
    
    val op2 = resolve(state.stack.pop, state)
    
    val rawOp1 = state.stack.pop
    
    rawOp1 match {
      case VarRef(name)  =>
        val theVar = state.vars.resolveId(name)
        val result = performTypedBinaryOperation(theVar.value, op2, binaryExpr.getOperator).getOrElse{
          performBinaryOperation(theVar.value.value, op2, binaryExpr.getOperator)
        }
        
        if (!result.isInstanceOf[Boolean] && TypeHelper.resolve(theVar.value.theType).isUnsigned) {
          TypeHelper.cast(TypeHelper.resolve(theVar.value.theType), result).value
        } else if (!result.isInstanceOf[Boolean] && TypeHelper.isPointer(theVar.theType)) {
          ValueInfo(Address(result.asInstanceOf[Int]), theVar.theType)
        } else {
          result
        }
      case _ =>
        val simple = resolve(rawOp1, state)
        performBinaryOperation(simple, op2, binaryExpr.getOperator)
    }
  }
}