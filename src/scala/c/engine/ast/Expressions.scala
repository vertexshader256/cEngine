package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.annotation.switch

object Expressions {

  def evaluate(expr: IASTInitializerClause)(implicit state: State): Option[ValueType] = expr match {
    case exprList: IASTExpressionList =>
      exprList.getExpressions.map{x => evaluate(x)}.last
    case ternary: IASTConditionalExpression =>
      val result = TypeHelper.resolveBoolean (evaluate(ternary.getLogicalConditionExpression).get)

      val expr = if (result) {
        ternary.getPositiveResultExpression
      } else {
        ternary.getNegativeResultExpression
      }

      evaluate(expr)
    case cast: IASTCastExpression =>
        val theType = TypeHelper.getType(cast.getTypeId).theType
        val operand = evaluate(cast.getOperand).get

        Some(operand match {
          case str @ StringLiteral(_) => str
          case LValue(addr, aType) =>
            theType match {
              case ptr: IPointerType if aType.isInstanceOf[IArrayType] =>
                val newAddr = state.allocateSpace(4)
                state.Stack.writeToMemory(addr, newAddr, theType)
                LValue(state, newAddr, theType)
              case _ => LValue(state, addr, theType)
            }
          case RValue(value, _) =>
            val newAddr = state.allocateSpace(TypeHelper.sizeof(theType))
            state.Stack.writeToMemory(TypeHelper.cast(theType, value).value, newAddr, theType)
            LValue(state, newAddr, theType)
        })
    case fieldRef: IASTFieldReference =>
        val struct = evaluate(fieldRef.getFieldOwner).get.asInstanceOf[LValue]

        val structType = TypeHelper.resolveStruct(struct.theType)

        val baseAddr = if (fieldRef.isPointerDereference) {
          state.readPtrVal(struct.address)
        } else {
          struct.address
        }

        val field = TypeHelper.offsetof(structType, baseAddr, fieldRef.getFieldName.toString, state: State)
        Some(field)
    case subscript: IASTArraySubscriptExpression =>

      val arrayVarPtr = evaluate(subscript.getArrayExpression).head.asInstanceOf[LValue]

      val base = TypeHelper.resolve(evaluate(subscript.getArgument).get)
      val index = TypeHelper.cast(TypeHelper.intType, base.value).value.asInstanceOf[Int]

      val aType = TypeHelper.getPointerType(arrayVarPtr.theType)

      arrayVarPtr.theType match {
        case x: IPointerType  =>
          val offset = state.readPtrVal(arrayVarPtr.address) + index * TypeHelper.sizeof(x.getType)
          Some(LValue(state, offset, aType))
        case x: IArrayType =>
          val offset = arrayVarPtr.address + index * TypeHelper.sizeof(x.getType)
          Some(LValue(state, offset, aType))
      }
    case unary: IASTUnaryExpression =>
      Some(UnaryExpression.execute(unary))
    case lit: IASTLiteralExpression =>
        Some(Literal.cast(lit.toString))
    case id: IASTIdExpression =>
        Some(state.context.resolveId(id.getName).get)
    case typeExpr: IASTTypeIdExpression =>
      // used for sizeof calls on a type
        val theType = TypeHelper.getType(typeExpr.getTypeId).theType
        Some(RValue(TypeHelper.sizeof(theType), TypeHelper.intType))
    case call: IASTFunctionCallExpression =>
        val pop = evaluate(call.getFunctionNameExpression).head

        val name = if (state.hasFunction(call.getFunctionNameExpression.getRawSignature)) {
          call.getFunctionNameExpression.getRawSignature
        } else {
          val info = pop.asInstanceOf[LValue]
          val resolved = TypeHelper.stripSyntheticTypeInfo(info.theType)
          resolved match {
            case ptr: IPointerType => state.getFunctionByIndex(info.rValue.value.asInstanceOf[Int]).name
          }
        }

        state.callTheFunction(name, call, None)
    case bin: IASTBinaryExpression =>
      val result = (bin.getOperator, evaluate(bin.getOperand1).head) match {
        case (IASTBinaryExpression.op_logicalOr, op1 @ RValue(x: Boolean, _)) if x => op1
        case (IASTBinaryExpression.op_logicalAnd, op1 @ RValue(x: Boolean, _)) if !x => op1
        case (_, op1) =>
          val op2 = evaluate(bin.getOperand2).head

          val result = if (Utils.isAssignment(bin.getOperator)) {
            Declarator.assign(op1.asInstanceOf[LValue], List(op2), bin.getOperand2, bin.getOperator)
          } else {
            BinaryExpr.evaluate(op1, op2, bin.getOperator)
          }

          result
      }

      Some(result)
  }
}