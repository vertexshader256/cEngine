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
          new RValue(~value.asInstanceOf[RValue].value.asInstanceOf[Int], null) {}
        case `op_not` => new RValue(TypeHelper.not(value), TypeHelper.one.theType) {}
        case `op_minus` =>
          val valueType = value
          val newVal = BinaryExpr.evaluate(unary, valueType, TypeHelper.negativeOne, IASTBinaryExpression.op_multiply).value
          new RValue(newVal, valueType.theType) {}
        case `op_postFixIncr` =>
          val lValue = value.asInstanceOf[LValue]
          val newVal = BinaryExpr.evaluate(unary, lValue, TypeHelper.one, IASTBinaryExpression.op_plus).value

          // push then set
          val returnVal = new RValue(lValue.value.value, lValue.theType) {}
          state.Stack.writeToMemory(newVal, lValue.address, lValue.theType)
          returnVal
        case `op_postFixDecr` =>
          val lValue = value.asInstanceOf[LValue]
          val newVal = BinaryExpr.evaluate(unary, lValue, TypeHelper.one, IASTBinaryExpression.op_minus).value

          // push then set
          val returnVal = new RValue(lValue.value.value, lValue.theType) {}
          state.Stack.writeToMemory(newVal, lValue.address, lValue.theType)
          returnVal
        case `op_prefixIncr` =>
          val lValue = value.asInstanceOf[LValue]
          val newVal = BinaryExpr.evaluate(unary, lValue, TypeHelper.one, IASTBinaryExpression.op_plus).value

          // set then push
          state.Stack.writeToMemory(newVal, lValue.address, lValue.theType)
          new RValue(newVal, lValue.theType) {}
        case `op_prefixDecr` =>
          val lValue = value.asInstanceOf[LValue]
          val newVal = BinaryExpr.evaluate(unary, lValue, TypeHelper.one, IASTBinaryExpression.op_minus).value

          // set then push
          state.Stack.writeToMemory(newVal, lValue.address, lValue.theType)
          new RValue(newVal, lValue.theType) {}
        case `op_sizeof` =>
          value match {
            case info @ LValue(_, theType) => new RValue(info.sizeof, new CBasicType(IBasicType.Kind.eInt, 0)) {}
          }
        case `op_amper` =>
          value match {
            case info@LValue(_, _) =>
              info.theType match {
                case fcn: CFunctionType => MemoryLocation(state, info.address, fcn)
                case x: IType => Address(info.address, state.pointerType, x)
              }
          }
        case `op_star` =>
          value match {
            case RValue(int: Int, theType) =>
              MemoryLocation(state, int, TypeHelper.resolve(theType))
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
