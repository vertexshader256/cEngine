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
          case AddressInfo(_, _) => stack.readVal(address)
          case VarRef(name) => 
            if (!context.vars.resolveId(name).isPointer) {
              // only if op1 is NOT a pointer, resolve op2
              stack.readVal(address)
            } else {
              address
            }
        }    
      case Address(address) => 
        op1 match {
          //case Address(_) => stack.readVal(address)
          case VarRef(name) => 
            if (!context.vars.resolveId(name).isPointer) {
              // only if op1 is NOT a pointer, resolve op2
              stack.readVal(address)
            } else {
              address
            }
        }  
      case int: Int => int
      case doub: Double => doub
      case float: Float => float
    }
    
    stack.setValue(resolvedop2, destinationAddress.address)

    resolvedop2
  }
  
  def parse(binaryExpr: IASTBinaryExpression, context: State, stack: State): Any = {
    import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._

    // read the two operands from right to left
    var op2: Any = context.stack.pop
    var op1: Any = context.stack.pop
    
    var isOp1Pointer = false
    var pointerType: IType = null
    
    // resolve literals
    
    op1 = op1 match {
      case lit @ Literal(_) => lit.cast
      case x => x
    }
    
    op2 = op2 match {
      case lit @ Literal(_) => lit.cast
      case x => x
    }
    
    def resolve(op: Any) = op match { 
      case VarRef(name) => 
        val theVar = context.vars.resolveId(name)
        if (theVar.isPointer) {
          isOp1Pointer = true
          pointerType = theVar.theType
          AddressInfo(Address(theVar.value.asInstanceOf[Int]), theVar.theType)
        } else {
          theVar.value  
        }
      case int: Int => int
      case bool: Boolean => bool
      case double: Double => double
      case float: Float => float
    }

    val op = binaryExpr.getOperator
    
    // resolve Op1 only if not assignment
    
    if (op != op_plusAssign &&
        op != op_minusAssign) {
      op1 = resolve(op1)
      op2 = resolve(op2)
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
        }
      case `op_divide` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x / y
          case (x: Double, y: Int) => x / y
          case (x: Int, y: Double) => x / y
          case (x: Double, y: Double) => x / y
        }
      //case `op_assign` =>
      //  parseAssign(op1, op2, context)
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
            vari.setValue((vari.value, op2) match {
              case (x: Int, y: Int) => x + y
              case (x: Double, y: Int) => x + y
              case (x: Int, y: Double) => x + y
              case (x: Double, y: Double) => x + y
            })
            
        }
      case `op_minusAssign` =>
        op1 match {
          case VarRef(name) => 
            val vari = context.vars.resolveId(name)
            vari.setValue((vari.value, op2) match {
              case (x: Int, y: Int) => x - y
              case (x: Double, y: Int) => x - y
              case (x: Int, y: Double) => x - y
              case (x: Double, y: Double) => x - y
            })
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