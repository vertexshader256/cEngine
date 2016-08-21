package app.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;

object BinaryExpression {
  
  def parse(binaryExpr: IASTBinaryExpression, direction: Direction, context: IASTContext): Any = {
    import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._
    
    if (direction == Exiting) {

      var op2: Any = context.stack.pop
      var op1: Any = context.stack.pop
      
      def resolve(op: Any) = op match {
        case VarRef(name) => 
          println("VARREF RESOLUTION: " + name + " : " + context.vars.resolveId(name).value)
          context.vars.resolveId(name).value
        case Address(typeName, addy) => 
          val result = Variable.readVal(addy, typeName)
          println("ADDRESS RESOLUTION: " + result)
          result
        case Variable(value) => 
          println("VARIABLE RESOLUTION: " + value)
          value
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
        case `op_assign` =>
          val destinationAddress: Address = op1 match {
            case vari @ Variable(_) =>
              vari.address
            case VarRef(name) =>
              context.vars.resolveId(name).address
            case addy @ Address(_,_) => addy
          }
          
          val resolvedop2 = op2 match {
            case VarRef(name) => 
              val theVar = context.vars.resolveId(name)
              if (theVar.refAddress != null) {
                theVar.refAddress
              } else {
                theVar.value
              }
            case int: Int => int
            case addy @ Address(_,_) => addy
            case doub: Double => doub
          }
          
          val dest = context.vars.resolveAddress(destinationAddress)
          
          if (dest.numElements > 1) {
            val index = (destinationAddress.address - dest.address.address) / TypeHelper.sizeof(dest.typeName)
            println("SETTING ARRAY INDEX: " + index + " : " + index.getClass.getSimpleName)
            dest.setArrayValue(resolvedop2, index)
          } else {
            dest.setValue(resolvedop2)
          }

          resolvedop2
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
    } else {
      null
    }
  }
}