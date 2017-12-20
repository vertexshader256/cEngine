package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.{CBasicType, CFunctionType}
import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression._
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._

object UnaryExpression {
  def execute(value: ValueType, unary: IASTUnaryExpression)(implicit state: State): ValueType = {

      unary.getOperator match {
        case `op_bracketedPrimary` => value
        case `op_tilde` =>
          RValue(~value.asInstanceOf[RValue].value.asInstanceOf[Int], null)
        case `op_not` => RValue(TypeHelper.not(value), TypeHelper.one.theType)
        case `op_minus` =>
          val valueType = value
          val newVal = BinaryExpr.evaluate(unary, valueType, TypeHelper.negativeOne, IASTBinaryExpression.op_multiply).value
          RValue(newVal, valueType.theType)
        case `op_postFixIncr` =>
          val lValue = value.asInstanceOf[LValue]
          val newVal = BinaryExpr.evaluate(unary, lValue, TypeHelper.one, IASTBinaryExpression.op_plus).value

          // push then set
          val returnVal = RValue(lValue.value.value, lValue.theType)
          state.Stack.writeToMemory(newVal, lValue.address, lValue.theType)
          returnVal
        case `op_postFixDecr` =>
          val lValue = value.asInstanceOf[LValue]
          val newVal = BinaryExpr.evaluate(unary, lValue, TypeHelper.one, IASTBinaryExpression.op_minus).value

          // push then set
          val returnVal = RValue(lValue.value.value, lValue.theType)
          state.Stack.writeToMemory(newVal, lValue.address, lValue.theType)
          returnVal
        case `op_prefixIncr` =>
          val lValue = value.asInstanceOf[LValue]
          val newVal = BinaryExpr.evaluate(unary, lValue, TypeHelper.one, IASTBinaryExpression.op_plus).value

          // set then push
          state.Stack.writeToMemory(newVal, lValue.address, lValue.theType)
          RValue(newVal, lValue.theType)
        case `op_prefixDecr` =>
          val lValue = value.asInstanceOf[LValue]
          val newVal = BinaryExpr.evaluate(unary, lValue, TypeHelper.one, IASTBinaryExpression.op_minus).value

          // set then push
          state.Stack.writeToMemory(newVal, lValue.address, lValue.theType)
          RValue(newVal, lValue.theType)
        case `op_sizeof` =>
          value match {
            case info @ LValue(_, theType) => RValue(info.sizeof, TypeHelper.pointerType)
          }
        case `op_amper` =>
          value match {
            case info@LValue(_, _) =>
              info.theType match {
                case fcn: CFunctionType => LValue(info.address, fcn)
                case x: IType => RValue(info.address, TypeHelper.pointerType)
              }
          }
        case `op_star` =>
          value match {
            case RValue(int: Int, theType) =>
              LValue(int, TypeHelper.resolve(theType))
            case info@LValue(_, _) =>
              val nestedType = info.theType match {
                case ptr: IPointerType => ptr.getType
                case array: IArrayType => array.getType
              }

              if (!nestedType.isInstanceOf[IFunctionType]) {
                LValue(info.value.value.asInstanceOf[Int], nestedType)
              } else {
                // function pointers can ignore the star
                info
              }
          }
      }
  }
}
