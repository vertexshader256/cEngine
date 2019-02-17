package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.{CBasicType, CFunctionType}
import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression._
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._

object UnaryExpression {

  // per C Spec this returns a RValue
  def evaluateIncrDecr(unary: IASTUnaryExpression, value: ValueType, operator: Int)(implicit state: State): RValue = operator match {
    case `op_postFixIncr` =>
      val newVal = BinaryExpr.evaluate(value, TypeHelper.one, IASTBinaryExpression.op_plus).value
      value match {
        case lValue: LValue =>
          // push then set
          val pre = lValue.value
          state.Stack.writeToMemory(newVal, lValue.address, lValue.theType)
          pre
      }
    case `op_postFixDecr` =>
      val newVal = BinaryExpr.evaluate(value, TypeHelper.one, IASTBinaryExpression.op_minus).value
      value match {
        case lValue: LValue =>
          // push then set
          val pre = lValue.value
          state.Stack.writeToMemory(newVal, lValue.address, lValue.theType)
          pre
      }
    case `op_prefixIncr` =>
      val newVal = BinaryExpr.evaluate(value, TypeHelper.one, IASTBinaryExpression.op_plus)
      value match {
        case lValue: LValue =>
          // set then push
          state.Stack.writeToMemory(newVal.value, lValue.address, lValue.theType)
          newVal
      }
    case `op_prefixDecr` =>
      val newVal = BinaryExpr.evaluate(value, TypeHelper.one, IASTBinaryExpression.op_minus)
      value match {
        case lValue @ LValue(_, _) =>
          // set then push
          state.Stack.writeToMemory(newVal.value, lValue.address, lValue.theType)
          newVal
      }
  }

  def execute(value: ValueType, unary: IASTUnaryExpression)(implicit state: State): ValueType = {

      unary.getOperator match {
        case `op_bracketedPrimary` => value
        case `op_tilde` =>

          value match {
            case RValue(int: Int, theType) =>
              RValue(~value.asInstanceOf[RValue].value.asInstanceOf[Int], new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_UNSIGNED))
            case info @ LValue(_, _) =>
              val theValue = info.value

              val result = theValue.value match {
                case byte: Byte => ~byte
                case int: Int => ~int
                case short: Short => ~short
                case long: Long => ~long
              }
              TypeHelper.cast(info.theType, result)
          }
        case `op_not` => RValue(TypeHelper.not(value), TypeHelper.one.theType)
        case `op_minus` =>
          val newVal = BinaryExpr.evaluate(value, TypeHelper.negativeOne, IASTBinaryExpression.op_multiply).value
          RValue(newVal, value.theType)
        case op @ (`op_postFixIncr` | `op_postFixDecr` | `op_prefixIncr` | `op_prefixDecr`) =>
          evaluateIncrDecr(unary, value, op)
        case `op_sizeof` =>
          value match {
            case info: LValue => RValue(info.sizeof, new CBasicType(IBasicType.Kind.eInt, 0))
            case RValue(_, theType) => RValue(TypeHelper.sizeof(theType), new CBasicType(IBasicType.Kind.eInt, 0))
          }
        case `op_amper` =>
          value match {
            case info@LValue(_, _) =>
              info.theType match {
                case fcn: CFunctionType => MemoryLocation(state, info.address, fcn)
                case theType: IType => Address(info.address, theType)
              }
          }
        case `op_star` =>
          value match {
            case RValue(int: Int, theType) =>
              MemoryLocation(state, int, theType)
            case info@LValue(_, _) =>
              val nestedType = info.theType match {
                case ptr: IPointerType => ptr.getType
                case array: IArrayType => array.getType
              }

              if (!nestedType.isInstanceOf[IFunctionType]) {
                MemoryLocation(state, info.value.value.asInstanceOf[Int], nestedType)
              } else {
                // function pointers can ignore the star
                info
              }
          }
      }
  }
}
