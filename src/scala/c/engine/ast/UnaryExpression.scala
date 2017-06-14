package c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.{CBasicType, CFunctionType}
import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression._
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._

object UnaryExpression {
  def execute(unary: IASTUnaryExpression)(implicit state: State) = {
    val one = RValue(1, new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_UNSIGNED))
    val negativeOne = RValue(-1, new CBasicType(IBasicType.Kind.eInt, 0))

    def not(theVal: Any): AnyVal = theVal match {
      case info @ LValue(_, _) => not(info.value)
      case RValue(theVal, _) => not(theVal)
      case int: Int => if (int == 0) 1 else 0
      case long: Long => if (long == 0) 1 else 0
      case bool: Boolean => !bool
      case char: char => if (char == 0) 1 else 0
    }

    unary.getOperator match {
      case `op_tilde` =>
        state.stack.push(RValue(~state.stack.pop.asInstanceOf[RValue].value.asInstanceOf[Int], null))
      case `op_not` => state.stack.push(RValue(not(state.stack.pop), one.theType))
      case `op_minus` =>
        val valueType = state.stack.pop
        val newVal = BinaryExpr.evaluate(unary, valueType, negativeOne, IASTBinaryExpression.op_multiply).value
        state.stack.push(RValue(newVal, valueType.theType))
      case `op_postFixIncr` =>
        val lValue = state.stack.pop.asInstanceOf[LValue]
        val newVal = BinaryExpr.evaluate(unary, lValue, one, IASTBinaryExpression.op_plus).value

        state.stack.push(RValue(lValue.value.value, lValue.theType))
        state.Stack.writeToMemory(newVal, lValue.address)
      case `op_postFixDecr` =>
        val lValue = state.stack.pop.asInstanceOf[LValue]
        val newVal = BinaryExpr.evaluate(unary, lValue, one, IASTBinaryExpression.op_minus).value

        // push then set
        state.stack.push(RValue(lValue.value.value, lValue.theType))
        state.Stack.writeToMemory(newVal, lValue.address)
      case `op_prefixIncr` =>
        val lValue = state.stack.pop.asInstanceOf[LValue]
        val newVal = BinaryExpr.evaluate(unary, lValue, one, IASTBinaryExpression.op_plus).value

        // set then push
        state.Stack.writeToMemory(newVal, lValue.address)
        state.stack.push(RValue(newVal, lValue.theType))
      case `op_prefixDecr` =>
        val lValue = state.stack.pop.asInstanceOf[LValue]
        val newVal = BinaryExpr.evaluate(unary, lValue, one, IASTBinaryExpression.op_minus).value

        // set then push
        state.Stack.writeToMemory(newVal, lValue.address)
        state.stack.push(RValue(newVal, lValue.theType))
      case `op_sizeof` =>
        state.stack.push(state.stack.pop match {
          case info @ LValue(_, theType) => RValue(info.sizeof, TypeHelper.pointerType)
        })
      case `op_amper` =>
        state.stack.pop match {
          case info @ LValue(_, _) =>
            info.theType match {
              case fcn: CFunctionType => state.stack.push(LValue(info.address, fcn))
              case x: IType => state.stack.push(RValue(info.address, TypeHelper.pointerType))
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
              state.stack.push(LValue(info.value.value.asInstanceOf[Int], nestedType))
            } else {
              // function pointers can ignore the star
              state.stack.push(info)
            }

        }
      case `op_bracketedPrimary` => // not sure what this is for but I need it for weird stuff like (k*)++
    }
  }
}
