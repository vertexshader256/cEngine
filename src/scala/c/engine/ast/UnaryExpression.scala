package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.{CFunctionType}
import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression._

import scala.c.engine.ast.BinaryExpr.evaluatePointerArithmetic
import scala.c.engine.ast.Expressions.evaluate

object UnaryExpression {

  // per C Spec this returns a RValue
  def evaluateIncrDecr(unary: IASTUnaryExpression, value: ValueType, operator: Int)(implicit state: State): RValue = {
    val op = operator match {
      case `op_postFixIncr` | `op_prefixIncr` => IASTBinaryExpression.op_plus
      case `op_postFixDecr` | `op_prefixDecr` => IASTBinaryExpression.op_minus
    }

    value match {
      case lValue: LValue =>

        val newVal = if (TypeHelper.isPointerOrArray(lValue.theType)) {
          evaluatePointerArithmetic(lValue, 1, op)
        } else {
          BinaryExpr.evaluate(value, TypeHelper.one, op)
        }

        val pre = lValue.rValue
        state.Stack.writeToMemory(newVal.value, lValue.address, lValue.theType)

        operator match {
          case `op_postFixIncr` | `op_postFixDecr` =>
            // push then set
            pre
          case `op_prefixIncr` | `op_prefixDecr` =>
            // set then push
            newVal
        }
    }
  }

  def execute(unary: IASTUnaryExpression)(implicit state: State): ValueType = {
      val value = evaluate(unary.getOperand).head

    unary.getOperator match {
        case `op_bracketedPrimary` => value
        case `op_tilde` =>

          value match {
            case RValue(rValue, _) =>
              RValue(~rValue.asInstanceOf[Int], TypeHelper.unsignedIntType)
            case info@LValue(_, _) =>
              val theValue = info.rValue

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
          BinaryExpr.evaluate(value, TypeHelper.negativeOne, IASTBinaryExpression.op_multiply)
        case op@(`op_postFixIncr` | `op_postFixDecr` | `op_prefixIncr` | `op_prefixDecr`) =>
          evaluateIncrDecr(unary, value, op)
        case `op_sizeof` =>

            val size = value match {
              case info: LValue => info.sizeof
              case Address(_, _) =>
                state.pointerSize match {
                  case SixtyFourBits => 8
                  case ThirtyTwoBits => 4
                }
              case RValue(_, theType) => TypeHelper.sizeof(theType)
            }

            RValue(size, TypeHelper.intType)
        case `op_amper` =>
          value match {
            case info @ LValue(_, _) => // address-of operator requires an LValue
              info.theType match {
                case _: CFunctionType => info
                case theType: IType => Address(info.address, theType)
              }
          }
        case `op_star` =>
          value match {
            case RValue(int: Int, theType) =>
              LValue(state, int, theType)
            case info@LValue(_, aType) =>
              val nestedType = TypeHelper.getPointerType(aType)

              if (!nestedType.isInstanceOf[IFunctionType]) {
                LValue(state, info.rValue.value.asInstanceOf[Int], nestedType)
              } else {
                // function pointers can ignore the star
                info
              }
          }
      }
  }
}
