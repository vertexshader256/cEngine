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
      
      def resolveOp1() = op1 = op1 match {
        case VarRef(name) => context.vars.resolveId(name).value
        case Variable(value) => value
        case int: Int => int
        case bool: Boolean => bool
        case double: Double => double
      }
      
      def resolveOp2() = op2 = op2 match {
        case VarRef(name) => context.vars.resolveId(name).value
        case Variable(value) => value
        case int: Int => int
        case bool: Boolean => bool
        case double: Double => double
      }
      
      val op = binaryExpr.getOperator
      
      // resolve Op1 only if not assignment
      
      if (op != op_assign &&
          op != op_plusAssign &&
          op != op_minusAssign) {
        resolveOp1()
      }
      
      resolveOp2()

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
          val newVal = op1 match {
            case variable: Variable =>
              variable
            case VarRef(name) =>
              context.vars.resolveId(name)
          }
          
          println("SETTING " + newVal.name + " to " + newVal.value)
          newVal.setValue(op2)
          op2
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