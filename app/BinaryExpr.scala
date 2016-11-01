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
      case AddressInfo(addr, theType) => 
        if (!TypeHelper.isPointer(dst.theType)) {
          // only if op1 is NOT a pointer, resolve op2
          state.readVal(addr, TypeHelper.resolve(dst.theType))
        } else if (TypeHelper.isPointer(theType) && !Utils.isNestedPointer(dst.theType)) {
          state.readVal(addr, new CBasicType(IBasicType.Kind.eInt, 0))
        } else {
          addr.value
        }
      case Address(addy) => addy
      case long: Long => long
      case int: Int => int
      case doub: Double => doub
      case float: Float => float 
    }
    
    val theType = if (TypeHelper.isPointer(dst.theType)) {
      TypeHelper.pointerType
    } else {
      TypeHelper.resolve(dst.theType)
    }
    
    val theVal = state.readVal(dst.address, theType)
    
    val result = op match {
      case `op_plusAssign` =>
        performBinaryOperation(theVal, resolvedop2, op_plus, theType)
      case `op_minusAssign` =>
        performBinaryOperation(theVal, resolvedop2, op_minus, theType)
      case `op_multiplyAssign` =>
        performBinaryOperation(theVal, resolvedop2, op_multiply, theType)
      case `op_binaryXorAssign` =>
        performBinaryOperation(theVal, resolvedop2, op_binaryXor, theType)
      case `op_shiftRightAssign` =>
        performBinaryOperation(theVal, resolvedop2, op_shiftRight, theType)
      case `op_assign` =>
        resolvedop2
    }  
    
    state.setValue(result, dst)
    
    resolvedop2
  }
  
  private def resolveOperand(op: Any, context: State): AnyVal = {
    op match {
      case lit @ Literal(_) => lit.cast
      case Primitive(theVal, _) => theVal
      case VarRef(name)  =>      
        val theVar = context.vars.resolveId(name)
        theVar.value  
      case AddressInfo(addy, theType) =>
        context.readVal(addy, TypeHelper.resolve(theType))
      case int: Int => int
      case float: Float => float
      case double: Double => double
      case short: Short => short
      case char: Char => char
      case boolean: Boolean => boolean
      case long: Long => long
    }
  }
  
  def performBinaryOperation(op1: AnyVal, op2: AnyVal, operator: Int, destType: IType): AnyVal = {
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
          case (x: Int, y: Char) => x + y
          case (x: Int, y: Int) if (TypeHelper.isPointer(destType)) => x + y * TypeHelper.sizeof(TypeHelper.resolve(destType))
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
          case (x: Int, y: Int) if (TypeHelper.isPointer(destType)) => x - y * TypeHelper.sizeof(TypeHelper.resolve(destType))
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
      case `op_shiftLeft` =>
        (op1, op2) match {
          case (x: Long, y: Int) => x << y
          case (x: Int, y: Int) => x << y
        }
      case `op_equals` =>
        (op1, op2) match {
          case (x: Char, y: Int) => x == y
          case (x: Int, y: Int) => x == y
          case (x: Char, y: Char) => x == y
          case (x: Double, y: Int) => x == y
          case (x: Int, y: Double) => x == y
          case (x: Double, y: Double) => x == y
        }
      case `op_notequals` =>
        !performBinaryOperation(op1, op2, op_equals, destType).asInstanceOf[Boolean]
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
    val op1Raw = state.stack.pop
    val op1 = resolveOperand(op1Raw, state)
  
    if (op1Raw.isInstanceOf[VarRef]) {
        val theVar = state.vars.resolveId(op1Raw.asInstanceOf[VarRef].name) 
        val result = performBinaryOperation(op1, op2, binaryExpr.getOperator, theVar.theType)
        
        // HACKY: figure out how to better deal with unsigned binary ops
        if (result.isInstanceOf[Long] && TypeHelper.resolve(theVar.theType).isUnsigned) {
          TypeHelper.cast(TypeHelper.resolve(theVar.theType), result)
        } else {
          result
        }
    } else {
      performBinaryOperation(op1, op2, binaryExpr.getOperator, null)
    }
  }
}