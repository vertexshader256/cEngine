package app.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._

object BinaryExpr {
  
  def parseAssign(op: Int, op1: Any, op2: Any, context: State, stack: State): Any = {

    val dst: AddressInfo = op1 match {
      case VarRef(name) =>
        val variable = context.vars.resolveId(name)
        variable.info
      case info @ AddressInfo(_, _) => info
    }

    val resolvedop2 = resolveOperand(op2, context) match {
      case AddressInfo(addy, theType) => 
        val address = addy.value
        if (!TypeHelper.isPointer(dst.theType)) {
          // only if op1 is NOT a pointer, resolve op2
          stack.readVal(address, TypeHelper.resolve(dst.theType))
        } else {
          address
        }
      case long: Long => long
      case int: Int => int
      case doub: Double => doub
      case float: Float => float 
    }
    
    val theVal = stack.readVal(dst.address.value, TypeHelper.resolve(dst.theType))
    
    op match {
      case `op_plusAssign` =>
        context.setValue((theVal, resolvedop2) match {
          case (x: Int, y: Int) => x + y
          case (x: Double, y: Int) => x + y
          case (x: Int, y: Double) => x + y
          case (x: Double, y: Double) => x + y
        }, dst)
      case `op_minusAssign` =>
        context.setValue((theVal, resolvedop2) match {
          case (x: Int, y: Int) => x - y
          case (x: Double, y: Int) => x - y
          case (x: Int, y: Double) => x - y
          case (x: Double, y: Double) => x - y
        }, dst)
      case `op_multiplyAssign` =>
        context.setValue((theVal, resolvedop2) match {
          case (x: Int, y: Int) => x * y
          case (x: Double, y: Int) => x * y
          case (x: Int, y: Double) => x * y
          case (x: Double, y: Double) => x * y
        }, dst)
      case `op_binaryXorAssign` =>
        context.setValue((theVal, resolvedop2) match {
          case (x: Int, y: Int) => x ^ y
        }, dst)
      case `op_assign` =>
        stack.setValue(resolvedop2, dst)
    }  

    resolvedop2
  }
  
  private def resolveOperand(op: Any, context: State) = {
    op match {
      case lit @ Literal(_) => lit.cast
      case VarRef(name)  =>      
        val theVar = context.vars.resolveId(name)
        if (TypeHelper.isPointer(theVar.theType)) {
          AddressInfo(Address(theVar.value.asInstanceOf[Int]), theVar.theType)
        } else {
          theVar.value  
        }
      case x => x
    }
  }
  
  def parse(binaryExpr: IASTBinaryExpression, context: State, stack: State): Any = {
    import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._

    val op = binaryExpr.getOperator
    
    // resolve literals
    
    val op2 = resolveOperand(context.stack.pop, context)
    val op1 = resolveOperand(context.stack.pop, context)
  
    val result: Any = binaryExpr.getOperator match {
      case `op_multiply` =>
        (op1, op2) match {
          case (x: Int, y: Char) => x * y
          case (x: Int, y: Short) => x * y
          case (x: Int, y: Int) => x * y
          case (x: Int, y: Float) => x * y
          case (x: Int, y: Double) => x * y
          case (x: Int, y: Long) => x * y
          
          case (x: Char, y: Char) => x * y
          case (x: Char, y: Short) => x * y
          case (x: Char, y: Int) => x * y
          case (x: Char, y: Float) => x * y
          case (x: Char, y: Long) => x * y
          case (x: Char, y: Double) => x * y
          
          case (x: Float, y: Char) => x * y
          case (x: Float, y: Short) => x * y
          case (x: Float, y: Int) => x * y
          case (x: Float, y: Double) => x * y
          case (x: Float, y: Float) => x * y
          case (x: Float, y: Long) => x * y
          
          case (x: Double, y: Char) => x * y
          case (x: Double, y: Short) => x * y          
          case (x: Double, y: Int) => x * y
          case (x: Double, y: Double) => x * y
          case (x: Double, y: Float) => x * y
          case (x: Double, y: Long) => x * y
          
          case (x: Long, y: Char) => x * y
          case (x: Long, y: Short) => x * y          
          case (x: Long, y: Int) => x * y
          case (x: Long, y: Float) => x * y
          case (x: Long, y: Double) => x * y
          case (x: Long, y: Long) => x * y
          
          case (x: Short, y: Char) => x * y         
          case (x: Short, y: Short) => x * y
          case (x: Short, y: Int) => x * y
          case (x: Short, y: Float) => x * y
          case (x: Short, y: Double) => x * y
          case (x: Short, y: Long) => x * y
        }
      case `op_plus` =>
        (op1, op2) match {
          case (AddressInfo(addy, theType), y: Int) => addy.value + y * TypeHelper.sizeof(theType)
          case (x: Int, y: Char) => x + y
          case (x: Int, y: Int) => x + y
          case (x: Int, y: Short) => x + y
          case (x: Int, y: Float) => x + y
          case (x: Int, y: Double) => x + y
          case (x: Int, y: Long) => x + y
          
          case (x: Char, y: Char) => x + y  
          case (x: Char, y: Short) => x + y          
          case (x: Char, y: Int) => x + y
          case (x: Char, y: Float) => x + y
          case (x: Char, y: Double) => x + y
          case (x: Char, y: Long) => x + y
          
          case (x: Float, y: Char) => x + y
          case (x: Float, y: Short) => x + y
          case (x: Float, y: Int) => x + y
          case (x: Float, y: Float) => x + y
          case (x: Float, y: Double) => x + y
          case (x: Float, y: Long) => x + y
          
          case (x: Double, y: Char) => x + y
          case (x: Double, y: Short) => x + y          
          case (x: Double, y: Int) => x + y
          case (x: Double, y: Double) => x + y
          case (x: Double, y: Float) => x + y
          case (x: Double, y: Long) => x + y
          
          case (x: Long, y: Char) => x + y
          case (x: Long, y: Short) => x + y
          case (x: Long, y: Int) => x + y
          case (x: Long, y: Float) => x + y
          case (x: Long, y: Double) => x + y
          case (x: Long, y: Long) => x + y
          
          case (x: Short, y: Char) => x + y
          case (x: Short, y: Short) => x + y
          case (x: Short, y: Int) => x + y
          case (x: Short, y: Float) => x + y
          case (x: Short, y: Double) => x + y
          case (x: Short, y: Long) => x + y
          
        }
      case `op_minus` =>
        (op1, op2) match {
          case (AddressInfo(addy, theType), y: Int) => addy.value - y * TypeHelper.sizeof(theType)
          case (x: Int, y: Char) => x - y
          case (x: Int, y: Short) => x - y
          case (x: Int, y: Int) => x - y
          case (x: Int, y: Float) => x - y
          case (x: Int, y: Double) => x - y
          case (x: Int, y: Long) => x - y
          
          case (x: Char, y: Char) => x - y
          case (x: Char, y: Short) => x - y         
          case (x: Char, y: Int) => x - y
          case (x: Char, y: Float) => x - y
          case (x: Char, y: Double) => x - y
          case (x: Char, y: Long) => x - y
          
          case (x: Float, y: Char) => x - y
          case (x: Float, y: Short) => x - y
          case (x: Float, y: Int) => x - y
          case (x: Float, y: Double) => x - y
          case (x: Float, y: Float) => x - y
          case (x: Float, y: Long) => x - y
          
          case (x: Double, y: Char) => x - y
          case (x: Double, y: Short) => x - y
          case (x: Double, y: Int) => x - y
          case (x: Double, y: Double) => x - y
          case (x: Double, y: Float) => x - y
          case (x: Double, y: Long) => x - y
          
          case (x: Long, y: Char) => x - y
          case (x: Long, y: Short) => x - y         
          case (x: Long, y: Int) => x - y
          case (x: Long, y: Float) => x - y
          case (x: Long, y: Double) => x - y
          case (x: Long, y: Long) => x - y
          
          case (x: Short, y: Short) => x - y
          case (x: Short, y: Int) => x - y
          case (x: Short, y: Char) => x - y
          case (x: Short, y: Float) => x - y
          case (x: Short, y: Double) => x - y
          case (x: Short, y: Long) => x - y
        }
      case `op_divide` =>
        val result: Double = (op1, op2) match {
          case (x: Int, y: Char) => x / y
          case (x: Int, y: Short) => x / y
          case (x: Int, y: Int) => x / y
          case (x: Int, y: Float) => x / y
          case (x: Int, y: Double) => x / y
          case (x: Int, y: Long) => x / y
          
          case (x: Char, y: Char) => x / y
          case (x: Char, y: Short) => x / y
          case (x: Char, y: Int) => x / y
          case (x: Char, y: Float) => x / y
          case (x: Char, y: Double) => x / y
          case (x: Char, y: Long) => x / y

          case (x: Float, y: Char) => x / y
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
 
          case (x: Long, y: Char) => x / y
          case (x: Long, y: Short) => x / y
          case (x: Long, y: Int) => x / y
          case (x: Long, y: Float) => x / y
          case (x: Long, y: Double) => x / y
          case (x: Long, y: Long) => x / y

          case (x: Short, y: Char) => x / y
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
      case `op_equals` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x == y
          case (x: Double, y: Int) => x == y
          case (x: Int, y: Double) => x == y
          case (x: Double, y: Double) => x == y
        }
      case `op_notequals` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x != y
          case (x: Double, y: Int) => x != y
          case (x: Int, y: Double) => x != y
          case (x: Double, y: Double) => x != y
        }
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
          case (x: Double, y: Int) => x >= y
          case (x: Int, y: Double) => x >= y
          case (x: Double, y: Double) => x >= y
        }  
      case `op_lessThan` =>
        (op1, op2) match {
          case (AddressInfo(addy, theType), y: Int) => addy.value < y
          case (AddressInfo(addy, theType), AddressInfo(addy2, theType2)) => addy.value < addy2.value
          case (x: Int, y: Int) => x < y
          case (x: Double, y: Int) => x < y
          case (x: Int, y: Double) => x < y
          case (x: Double, y: Double) => x < y
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
          case (x: Char, y: Int) => x ^ y
          case (x: Int, y: Char) => x ^ y
        }   
      case `op_binaryAnd` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x & y
          case (x: Char, y: Int) => x & y
          case (x: Int, y: Char) => x & y
        }
      case `op_logicalAnd` =>
        (op1, op2) match {
          case (x: Boolean, y: Boolean) => x && y
        }
      case `op_logicalOr` =>
        (op1, op2) match {
          case (x: Boolean, y: Boolean) => x || y
        }
      case _ => throw new Exception("unhandled binary operator: " + binaryExpr.getOperator); null
    }
    
    result
  }
}