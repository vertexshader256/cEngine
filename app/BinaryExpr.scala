package app.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;

object BinaryExpr {
  
  def parseAssign(op1: Any, op2: Any, context: IASTContext): Any = {
    val destinationAddress: Address = op1 match {
      case VarRef(name) =>
        context.vars.resolveId(name).address
      case addy @ Address(_,_) => addy
    }
    
    val resolvedop2 = op2 match {
      case VarRef(name) => 
        context.vars.resolveId(name).value
      case Address(address, typeName) => 
        op1 match {
          case Address(_,_) => Variable.readVal(address, typeName)
          case VarRef(name) => 
            if (!context.vars.resolveId(name).isPointer) {
              // only if op1 is NOT a pointer, resolve op2
              Variable.readVal(address, typeName)
            } else {
              address
            }
        }  
      case int: Int => int
      case doub: Double => doub
    }
    
    Variable.setValue(resolvedop2, destinationAddress.address)

    resolvedop2
  }
  
  def parse(binaryExpr: IASTBinaryExpression, context: IASTContext): Any = {
    import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._

    // read the two operands from right to left
    var op2: Any = context.stack.pop
    var op1: Any = context.stack.pop
    
    def resolve(op: Any) = op match {
      case VarRef(name) => 
        context.vars.resolveId(name).value
      case Address(addy, typeName) => 
        val result = Variable.readVal(addy, typeName)
        result
      case int: Int => int
      case bool: Boolean => bool
      case double: Double => double
    }
    
    def resolveOp1() = { 
      op1 = resolve(op1)
    }
    
    def resolveOp2() = { 
      op2 = resolve(op2)
    }
    
    val op = binaryExpr.getOperator
    
    // resolve Op1 only if not assignment
    
    if (op != op_assign &&
        op != op_plusAssign &&
        op != op_minusAssign) {
      resolveOp1()
      resolveOp2()
    }
    
    binaryExpr.getOperator match {
      case `op_multiply` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x * y
          case (x: Double, y: Int) => x * y
          case (x: Int, y: Double) => x * y
          case (x: Double, y: Double) => x * y
        }
      case `op_plus` =>
        (op1, op2) match {
          case (x: Int, y: Int) => x + y
          case (x: Double, y: Int) => x + y
          case (x: Int, y: Double) => x + y
          case (x: Double, y: Double) => x + y
        }
      case `op_minus` =>
        (op1, op2) match {
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
  }
}