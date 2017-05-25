package scala.c_engine

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.{CBasicType, CFunctionType}
import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression._
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._

object UnaryExpression {
  def execute(unary: IASTUnaryExpression)(implicit state: State) = {
    val one = RValue(1, new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_UNSIGNED))

    def not(theVal: Any): AnyVal = theVal match {
      case info @ LValue(_, _) => not(info.value)
      case RValue(theVal, _) => not(theVal)
      case int: Int               => if (int == 0) 1 else 0
      case bool: Boolean => !bool
      case char: char => if (char == 0) 1 else 0
    }

    unary.getOperator match {
      case `op_tilde` =>
        state.stack.push(RValue(~state.stack.pop.asInstanceOf[RValue].value.asInstanceOf[Int], null))
      case `op_not` => state.stack.push(RValue(not(state.stack.pop), one.theType))
      case `op_minus` =>
        state.stack.pop match {
          case RValue(int: Int, theType)     => state.stack.push(RValue(-int, theType))
          case RValue(doub: Double, theType)     => state.stack.push(RValue(-doub, theType))
          case info @ LValue(_, _) =>

            val basicType = info.theType.asInstanceOf[IBasicType]
            state.stack.push(basicType.getKind match {
              case `eInt`    => RValue(-info.value.value.asInstanceOf[Int], basicType)
              case `eDouble` => RValue(-info.value.value.asInstanceOf[Double], basicType)
            })
        }
      case `op_postFixIncr` =>
        val info = state.stack.pop.asInstanceOf[LValue]
        val newVal = BinaryExpr.evaluate(unary, info.value, one, IASTBinaryExpression.op_plus)

        state.stack.push(info.value)
        state.setValue(newVal.value, info.address)
      case `op_postFixDecr` =>
        val info = state.stack.pop.asInstanceOf[LValue]
        val newVal = BinaryExpr.evaluate(unary, info.value, one, IASTBinaryExpression.op_minus)

        // push then set
        state.stack.push(info.value)
        state.setValue(newVal.value, info.address)
      case `op_prefixIncr` =>
        val info = state.stack.pop.asInstanceOf[LValue]
        val newVal = BinaryExpr.evaluate(unary, info.value, one, IASTBinaryExpression.op_plus)

        // set then push
        state.setValue(newVal.value, info.address)
        state.stack.push(newVal)
      case `op_prefixDecr` =>
        val info = state.stack.pop.asInstanceOf[LValue]
        val newVal = BinaryExpr.evaluate(unary, info.value, one, IASTBinaryExpression.op_minus)

        // set then push
        state.setValue(newVal.value, info.address)
        state.stack.push(newVal)
      case `op_sizeof` =>
        state.stack.push(state.stack.pop match {
          case info @ LValue(_, theType) => RValue(info.sizeof, TypeHelper.pointerType)
        })
      case `op_amper` =>
        state.stack.pop match {
          case info @ LValue(_, _) =>
            info.theType match {
              case fcn: CFunctionType => state.stack.push(LValue(info.address, fcn))
              case x: IType => state.stack.push(RValue(info.address, x))
            }
        }
      case `op_star` =>
        state.stack.pop match {
          case RValue(int: Int, theType) =>
            state.stack.push(LValue(int, TypeHelper.resolve(theType)))
          case info @ LValue(_,_) =>
            val nestedType = info.theType match {
              case ptr: IPointerType => ptr.getType
              case array: IArrayType => array.getType
            }

            if (!nestedType.isInstanceOf[IFunctionType]) {

              if (info.theType.isInstanceOf[IArrayType]) {
                state.stack.push(LValue(info.address, nestedType))
              } else {
                val value = info.value
                state.stack.push(LValue(value.value.asInstanceOf[Int], nestedType))
              }

            } else {
              // function pointers can ignore the star
              state.stack.push(info)
            }

        }
      case `op_bracketedPrimary` => // not sure what this is for but I need it for weird stuff like (k*)++
    }
  }
}
