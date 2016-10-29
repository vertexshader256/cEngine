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
    }

    val resolvedop2 = op2 match {
      case lit @ Literal(_) => lit.cast
      case VarRef(name)  =>      
        val theVar = state.vars.resolveId(name)
        theVar.value
      case AddressInfo(addy, theType) => 
        val address = addy.value
        if (!TypeHelper.isPointer(dst.theType)) {
          // only if op1 is NOT a pointer, resolve op2
          state.readVal(address, TypeHelper.resolve(dst.theType))
        } else if (TypeHelper.isPointer(theType) && !Utils.isNestedPointer(dst.theType)) {
          state.readVal(address, new CBasicType(IBasicType.Kind.eInt, 0))
        } else {
          address
        }
      case Address(addy) => addy
      case long: Long => long
      case int: Int => int
      case doub: Double => doub
      case float: Float => float 
    }
    
    val theVal = if (TypeHelper.isPointer(dst.theType)) {
      state.readVal(dst.address.value, new CBasicType(IBasicType.Kind.eInt, 0))
    } else {
      state.readVal(dst.address.value, TypeHelper.resolve(dst.theType))
    }
    
    val result = op match {
      case `op_plusAssign` =>
        performBinaryOperation(theVal, resolvedop2, op_plus, state)
      case `op_minusAssign` =>
        performBinaryOperation(theVal, resolvedop2, op_minus, state)
      case `op_multiplyAssign` =>
        performBinaryOperation(theVal, resolvedop2, op_multiply, state)
      case `op_binaryXorAssign` =>
        performBinaryOperation(theVal, resolvedop2, op_binaryXor, state)
      case `op_shiftRightAssign` =>
        performBinaryOperation(theVal, resolvedop2, op_shiftRight, state)
      case `op_assign` =>
        resolvedop2
    }  
    
    state.setValue(result, dst)
    
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
  
  def performBinaryOperation(op1: Any, op2: Any, operator: Int, state: State): AnyVal = {
    operator match {
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
          case (AddressInfo(addy, theType), AddressInfo(addy2, theType2)) => addy.value == addy2.value
          case (AddressInfo(addy, theType), y: Int) => state.readVal(addy.value, TypeHelper.resolve(theType)) == y
          case (x: Char, y: Int) => x == y
          case (x: Int, y: Int) => x == y
          case (x: Char, y: Char) => x == y
          case (x: Double, y: Int) => x == y
          case (x: Int, y: Double) => x == y
          case (x: Double, y: Double) => x == y
        }
      case `op_notequals` =>
        !performBinaryOperation(op1, op2, op_equals, state).asInstanceOf[Boolean]
      case `op_greaterThan` =>
        (op1, op2) match {
          case (AddressInfo(addy, theType), AddressInfo(addy2, theType2)) => addy.value > addy2.value
          case (x: Int, y: Int) => x > y
          case (x: Double, y: Int) => x > y
          case (x: Int, y: Double) => x > y
          case (x: Double, y: Double) => x > y
        }
      case `op_greaterEqual` =>
        (op1, op2) match {
          case (AddressInfo(addy, theType), AddressInfo(addy2, theType2)) => addy.value >= addy2.value
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
          case (x: Boolean, y: Int) => x && (y > 0)
          case (x: Int, y: Boolean) => (x > 0) && y
        }
      case `op_logicalOr` =>
        (op1, op2) match {
          case (x: Boolean, y: Boolean) => x || y
          case (x: Int, y: Boolean) => (x > 0) || y
        }
      case _ => throw new Exception("unhandled binary operator: " + operator); 0
    }
  }
  
  def parse(binaryExpr: IASTBinaryExpression, state: State): Any = {

    val op2 = resolveOperand(state.stack.pop, state)
    val op1 = resolveOperand(state.stack.pop, state)
  
    performBinaryOperation(op1, op2, binaryExpr.getOperator, state)
  }
}