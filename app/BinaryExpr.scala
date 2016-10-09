package app.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;

object BinaryExpr {
  
  def parseAssign(op1: Any, op2: Any, context: State, stack: State): Any = {
    val destinationAddress: AddressInfo = op1 match {
      case VarRef(name) =>
        val variable = context.vars.resolveId(name)
        AddressInfo(variable.address, variable.theType)
      case info @ AddressInfo(addy, _) => info
    }
    
    // TODO: combine with resolve() function below?
    
    val resolvedop2 = op2 match {
      case lit @ Literal(_) => lit.cast
      case VarRef(name) => 
        context.vars.resolveId(name).value
      case AddressInfo(addy, _) => 
        val address = addy.value
        op1 match {
          case AddressInfo(_, theType) => stack.readVal(address, TypeHelper.resolve(theType))
          case VarRef(name) => 
            val variable = context.vars.resolveId(name)
            if (!TypeHelper.isPointer(variable.theType)) {
              // only if op1 is NOT a pointer, resolve op2
              stack.readVal(address, TypeHelper.resolve(variable.theType))
            } else {
              address
            }
        }   
      case long: Long => long
      case int: Int => int
      case doub: Double => doub
      case float: Float => float 
    }
    
    stack.setValue(resolvedop2, destinationAddress)

    resolvedop2
  }
  
  def parse(binaryExpr: IASTBinaryExpression, context: State, stack: State): Any = {
    import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._

    var isOp1Pointer = false
    var pointerType: IType = null
    
    val op = binaryExpr.getOperator
    
    // resolve literals
    
    val op2 = context.stack.pop match {
      case lit @ Literal(_) => lit.cast
      case VarRef(name) if (op != op_plusAssign &&
        op != op_minusAssign) => 
        val theVar = context.vars.resolveId(name)
        if (TypeHelper.isPointer(theVar.theType)) {
          AddressInfo(Address(theVar.value.asInstanceOf[Int]), theVar.theType)
        } else {
          theVar.value  
        }
      case x => x
    }
    
    val op1 = context.stack.pop match {
      case lit @ Literal(_) => lit.cast
      case VarRef(name) if (op != op_plusAssign &&
        op != op_minusAssign) => 
        val theVar = context.vars.resolveId(name)
        if (TypeHelper.isPointer(theVar.theType)) {
          isOp1Pointer = true
          pointerType = theVar.theType
          AddressInfo(Address(theVar.value.asInstanceOf[Int]), theVar.theType)
        } else {
          theVar.value  
        }
      case x => x
    }
  
    val result: Any = binaryExpr.getOperator match {
      case `op_multiply` =>
        println(op1.getClass.getSimpleName)
        (op1, op2) match {
          case (x: Int, y: Int) => x * y
          case (x: Double, y: Int) => x * y
          case (x: Int, y: Double) => x * y
          case (x: Double, y: Double) => x * y
          case (x: Float, y: Float) => x * y
        }
      case `op_plus` =>
        (op1, op2) match {
          case (AddressInfo(addy, theType), y: Int) => addy.value + y * TypeHelper.sizeof(theType)
          case (x: Int, y: Int) => x + y
          case (x: Double, y: Int) => x + y
          case (x: Int, y: Double) => x + y
          case (x: Double, y: Double) => x + y
        }
      case `op_minus` =>
        (op1, op2) match {
          case (AddressInfo(addy, theType), y: Int) => addy.value - y * TypeHelper.sizeof(theType)
          case (x: Int, y: Int) => x - y
          case (x: Double, y: Int) => x - y
          case (x: Int, y: Double) => x - y
          case (x: Double, y: Double) => x - y
          case (x: Long, y: Int) => x - y
          case (x: Int, y: Long) => x - y
          case (x: Float, y: Float) => x - y
        }
      case `op_divide` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x / y
          case (x: Double, y: Int) => x / y
          case (x: Int, y: Double) => x / y
          case (x: Double, y: Double) => x / y
        }
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
      case `op_lessThan` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x < y
          case (x: Double, y: Int) => x < y
          case (x: Int, y: Double) => x < y
          case (x: Double, y: Double) => x < y
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
      case `op_plusAssign` =>
        op1 match {
          case VarRef(name) => 
            val vari = context.vars.resolveId(name)
            context.setValue((vari.value, op2) match {
              case (x: Int, y: Int) => x + y
              case (x: Double, y: Int) => x + y
              case (x: Int, y: Double) => x + y
              case (x: Double, y: Double) => x + y
            }, vari.info)
            
        }
      case `op_minusAssign` =>
        op1 match {
          case VarRef(name) => 
            val vari = context.vars.resolveId(name)
            context.setValue((vari.value, op2) match {
              case (x: Int, y: Int) => x - y
              case (x: Double, y: Int) => x - y
              case (x: Int, y: Double) => x - y
              case (x: Double, y: Double) => x - y
            }, vari.info)
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
    
    if (isOp1Pointer) {
      AddressInfo(Address(result.asInstanceOf[Int]), pointerType)
    } else {
      result
    }
    
  }
}