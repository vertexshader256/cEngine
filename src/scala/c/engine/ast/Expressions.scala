package c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.annotation.switch

object Expressions {

  def evaluate(expr: IASTInitializerClause)(implicit state: State): Seq[ValueType] = expr match {
    case exprList: IASTExpressionList =>
      exprList.getExpressions.flatMap{x => evaluate(x)}
    case ternary: IASTConditionalExpression =>
      val result = TypeHelper.resolveBoolean (evaluate(ternary.getLogicalConditionExpression).head)

      if (result) {
        evaluate(ternary.getPositiveResultExpression)
      } else {
        evaluate(ternary.getNegativeResultExpression)
      }
    case cast: IASTCastExpression =>
        val theType = TypeHelper.getType(cast.getTypeId).theType
        val operand = evaluate(cast.getOperand).head

        Seq(operand match {
          case str @ StringLiteral(_) => str
          case LValue(addr, aType) =>
            theType match {
              case ptr: IPointerType if aType.isInstanceOf[IArrayType] =>
                val newAddr = state.allocateSpace(4)
                state.Stack.writeToMemory(addr, newAddr, theType)
                LValue(newAddr, theType)
              case _ => LValue(addr, theType)
            }
          case RValue(value, _) =>
            val newAddr = state.allocateSpace(TypeHelper.sizeof(theType))
            state.Stack.writeToMemory(TypeHelper.cast(theType, value).value, newAddr, theType)
            LValue(newAddr, theType)
        })
    case fieldRef: IASTFieldReference =>
        var baseAddr = -1

        val struct = evaluate(fieldRef.getFieldOwner).head.asInstanceOf[LValue]

        def resolve(theType: IType, addr: Int): CStructure = theType match {
          case qual: CQualifierType =>
            resolve(qual.getType, addr)
          case typedef: CTypedef =>
            resolve(typedef.getType, addr)
          case struct: CStructure => struct
          case ptr: IPointerType =>
            resolve(ptr.getType, baseAddr)
        }

        val structType = resolve(struct.theType, struct.address)

        baseAddr = if (fieldRef.isPointerDereference) {
          state.readPtrVal(struct.address)
        } else {
          struct.address
        }

        var offset = 0
        var resultAddress: LValue = null
        structType.getFields.foreach{field =>
          if (field.getName == fieldRef.getFieldName.getRawSignature) {
            // can assume names are unique
            resultAddress = LValue(baseAddr + offset, field.getType)
          } else {
            offset += TypeHelper.sizeof(field.getType)
          }
        }

        Seq(resultAddress)
    case subscript: IASTArraySubscriptExpression =>
      val arrayVarPtr = evaluate(subscript.getArrayExpression).head.asInstanceOf[LValue]
      val index = evaluate(subscript.getArgument).head match {
        case x @ RValue(_, _) => TypeHelper.cast(TypeHelper.pointerType, x.value).value.asInstanceOf[Int]
        case info @ LValue(_, _) =>
          info.value.value match {
            case int: Int => int
            case long: Long => long.toInt
          }
      }

      var aType = arrayVarPtr.theType

      val offset =
        if (arrayVarPtr.isInstanceOf[ArrayVariable]) {
          aType = arrayVarPtr.asInstanceOf[ArrayVariable].theType.getType
          arrayVarPtr.address + index * TypeHelper.sizeof(aType)
        } else {
          aType match {
            case array: IArrayType =>
              aType = array.getType
              state.readPtrVal(arrayVarPtr.address) + index * TypeHelper.sizeof(aType)
            case ptr: IPointerType =>
              aType = ptr.getType
              state.readPtrVal(arrayVarPtr.address) + index * TypeHelper.sizeof(aType)
          }
        }

      aType = TypeHelper.stripSyntheticTypeInfo(aType)

      Seq(LValue(offset, aType))
    case unary: IASTUnaryExpression =>
      Seq(UnaryExpression.execute(evaluate(unary.getOperand).head, unary))
    case lit: IASTLiteralExpression =>
        val litStr = lit.getRawSignature
        Seq(if (litStr.head == '\"' && litStr.last == '\"') {
          StringLiteral(litStr)
        } else {
          Literal.cast(lit.getRawSignature)
        })
    case id: IASTIdExpression =>
        Seq(state.context.resolveId(id.getName).get)
    case typeExpr: IASTTypeIdExpression =>
      // used for sizeof calls on a type
        val theType = TypeHelper.getType(typeExpr.getTypeId).theType
        Seq(RValue(TypeHelper.sizeof(theType), TypeHelper.pointerType))
    case call: IASTFunctionCallExpression =>
        val pop = evaluate(call.getFunctionNameExpression).head

        val name = if (state.hasFunction(call.getFunctionNameExpression.getRawSignature)) {
          call.getFunctionNameExpression.getRawSignature
        } else {
          val info = pop.asInstanceOf[LValue]
          val resolved = TypeHelper.stripSyntheticTypeInfo(info.theType)
          resolved match {
            case ptr: IPointerType => state.getFunctionByIndex(info.value.value.asInstanceOf[Int]).name
          }
        }

        val args = call.getArguments.map{x => evaluate(x).head}

        println(args.toList)
        state.callTheFunction(name, call, args).map{ x => Seq(x)}.getOrElse(Seq())
    case bin: IASTBinaryExpression =>
      val result = (bin.getOperator, evaluate(bin.getOperand1).head) match {
        case (IASTBinaryExpression.op_logicalOr, op1 @ RValue(x: Boolean, _)) if x => op1
        case (IASTBinaryExpression.op_logicalAnd, op1 @ RValue(x: Boolean, _)) if !x => op1
        case (_, op1) =>
          val op2 = evaluate(bin.getOperand2).head

          val result = if (Utils.isAssignment(bin.getOperator)) {
            BinaryExpr.parseAssign(bin, bin.getOperator, op1.asInstanceOf[LValue], op2)
          } else {
            BinaryExpr.evaluate(bin, op1, op2, bin.getOperator)
          }

          result
      }

      Seq(result)
  }
}