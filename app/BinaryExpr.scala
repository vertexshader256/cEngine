package scala.astViewer

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
        case VarRef(name) => context.resolveId(name).value
        case Variable(_, value) => value
        case int: Int => int
        case bool: Boolean => bool
        case double: Double => double
      }
      
      def resolveOp2() = op2 = op2 match {
        case VarRef(name) => context.resolveId(name).value
        case Variable(_, value) => value
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
            case (x: Int, y: Int) =>
              x * y
            case (x: Double, y: Int) =>
              x * y
            case (x: Int, y: Double) =>
              x * y
            case (x: Double, y: Double) =>
              x * y
          }
        case `op_plus` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              x + y
            case (x: Double, y: Int) =>
              x + y
            case (x: Int, y: Double) =>
              x + y
            case (x: Double, y: Double) =>
              x + y
          }
        case `op_minus` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              x - y
            case (x: Double, y: Int) =>
              x - y
            case (x: Int, y: Double) =>
              x - y
            case (x: Double, y: Double) =>
              x - y
          }
        case `op_divide` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              x / y
            case (x: Double, y: Int) =>
              x / y
            case (x: Int, y: Double) =>
              x / y
            case (x: Double, y: Double) =>
              x / y
          }
        case `op_assign` =>
          op1 match {
            case variable: Variable =>
              variable.value = op2
            case VarRef(name) =>
              context.getVariable(name).value = op2
          }

          op2
        case `op_equals` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              x == y
            case (x: Double, y: Int) =>
              x == y
            case (x: Int, y: Double) =>
              x == y
            case (x: Double, y: Double) =>
              x == y
          }
        case `op_greaterThan` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              x > y
            case (x: Double, y: Int) =>
              x > y
            case (x: Int, y: Double) =>
              x > y
            case (x: Double, y: Double) =>
              x > y
          }
        case `op_lessThan` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              x < y
            case (x: Double, y: Int) =>
              x < y
            case (x: Int, y: Double) =>
              x < y
            case (x: Double, y: Double) =>
              x < y
          }
        case `op_plusAssign` =>
          op1 match {
            case VarRef(name) => context.getVariable(name).value = context.getVariable(name).value.asInstanceOf[Int] + op2.asInstanceOf[Int]
          }
        case `op_minusAssign` =>
          op1 match {
            case VarRef(name) => context.getVariable(name).value = context.getVariable(name).value.asInstanceOf[Int] - op2.asInstanceOf[Int]
          }
        case `op_logicalAnd` =>
          (op1, op2) match {
            case (x: Boolean, y: Boolean) =>
              x && y
          }
        case `op_logicalOr` =>
          (op1, op2) match {
            case (x: Boolean, y: Boolean) =>
              x || y
          }
        case _ => throw new Exception("unhandled binary operator"); null
      }
    } else {
      null
    }
  }
}