package c.engine

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.annotation.switch

object Expressions {

  def parse(expr: IASTExpression, direction: Direction)(implicit context: State): Seq[IASTNode] = expr match {
    case exprList: IASTExpressionList => direction match {
      case Entering => exprList.getExpressions
      case Exiting => Seq ()
    }
    case ternary: IASTConditionalExpression => direction match {
      case Entering => Seq (ternary.getLogicalConditionExpression)
      case Exiting =>
        val result = TypeHelper.resolveBoolean (context.stack.pop)

        if (result) {
          Seq (ternary.getPositiveResultExpression)
        } else {
          Seq (ternary.getNegativeResultExpression)
        }
    }
    case cast: IASTCastExpression => direction match {
      case Entering => Seq(cast.getOperand, cast.getTypeId)
      case Exiting =>
        val theType = context.stack.pop.asInstanceOf[TypeInfo].value
        val operand = context.stack.pop

        context.stack.push(operand match {
          case str @ StringLiteral(_) => str
          case LValue(addr, _) => LValue(addr, theType)
          case RValue(value, _) =>
            val newAddr = context.allocateSpace(TypeHelper.sizeof(theType))
            context.setValue(TypeHelper.cast(theType, value).value, newAddr)
            LValue(newAddr, theType)
        })

        Seq()
    }
    case fieldRef: IASTFieldReference => direction match {
      case Entering => Seq(fieldRef.getFieldOwner)
      case Exiting =>
        var baseAddr = -1

        val struct = context.stack.pop.asInstanceOf[LValue]

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
          context.readPtrVal(struct.address)
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

        context.stack.push(resultAddress)

        Seq()
    }
    case subscript: IASTArraySubscriptExpression => direction match {
      case Entering => Seq(subscript.getArrayExpression, subscript.getArgument)
      case Exiting =>
        val index = context.stack.pop match {
          case x @ RValue(_, _) => TypeHelper.cast(TypeHelper.pointerType, x.value).value.asInstanceOf[Int]
          case info @ LValue(_, _) =>
            info.value.value match {
              case int: Int => int
              case long: Long => long.toInt
            }
        }

        val arrayVarPtr = context.stack.pop.asInstanceOf[LValue]
        var aType = arrayVarPtr.theType

        val offset =
          if (arrayVarPtr.isInstanceOf[ArrayVariable]) {
            aType = arrayVarPtr.asInstanceOf[ArrayVariable].theType.getType
            arrayVarPtr.address + index * TypeHelper.sizeof(aType)
          } else {
            aType match {
              case array: IArrayType =>
                aType = array.getType
                context.readPtrVal(arrayVarPtr.address) + index * TypeHelper.sizeof(aType)
              case ptr: IPointerType =>
                aType = ptr.getType
                context.readPtrVal(arrayVarPtr.address) + index * TypeHelper.sizeof(aType)
            }
          }

        aType = TypeHelper.stripSyntheticTypeInfo(aType)

        context.stack.push(LValue(offset, aType))

        Seq()
    }
    case unary: IASTUnaryExpression => direction match {
      case Entering => Seq(unary.getOperand)
      case Exiting =>
        UnaryExpression.execute(unary)
        Seq()
      case Gotoing => Seq()
    }
    case lit: IASTLiteralExpression =>
      if (direction == Exiting) {
        //println("PUSHING LIT: " + castLiteral(lit))

        val litStr = lit.getRawSignature
        if (litStr.head == '\"' && litStr.last == '\"') {
          context.stack.push(StringLiteral(litStr))
        } else {
          context.stack.push(Literal.cast(lit.getRawSignature))
        }
      }
      Seq()
    case id: IASTIdExpression =>
      if (direction == Exiting) {
        context.stack.push(context.context.resolveId(id.getName.getRawSignature))
      }
      Seq()
    case typeExpr: IASTTypeIdExpression =>
      // used for sizeof calls on a type
      if (direction == Entering) {
        Seq(typeExpr.getTypeId)
      } else {
        val theType = context.stack.pop.asInstanceOf[TypeInfo].value
        context.stack.push(RValue(TypeHelper.sizeof(theType), TypeHelper.pointerType))
        Seq()
      }
    case call: IASTFunctionCallExpression => direction match {
      case Entering => call.getArguments.reverse ++ Seq(call.getFunctionNameExpression)
      case Exiting =>
        val pop = context.stack.pop

        val name = if (context.hasFunction(call.getFunctionNameExpression.getRawSignature)) {
          call.getFunctionNameExpression.getRawSignature
        } else {
          val info = pop.asInstanceOf[LValue]
          val resolved = TypeHelper.stripSyntheticTypeInfo(info.theType)
          resolved match {
            case fcn: IFunctionType => context.getFunctionByIndex(info.address).name
            case ptr: IPointerType => context.getFunctionByIndex(info.value.value.asInstanceOf[Int]).name
          }
        }

        val args = call.getArguments.map{x => context.stack.pop}

        context.callTheFunction(name, call, args)
      case Gotoing => Seq()
    }
    case bin: IASTBinaryExpression => direction match {
      case Entering => Seq(bin.getOperand1)
      case Exiting =>
        if (context.context.visited.contains(bin.getOperand2)) {
          val result = if (Utils.isAssignment(bin.getOperator)) {
            val op2 = context.stack.pop
            val op1 = context.stack.pop
            BinaryExpr.parseAssign(bin, bin.getOperator, op1, op2)
          } else {
            val op2 = context.stack.pop
            val op1 = context.stack.pop

            BinaryExpr.evaluate(bin, op1, op2, bin.getOperator)
          }

          context.stack.push(result)
          Seq()
        } else {
          // short circuiting
          if (bin.getOperator == IASTBinaryExpression.op_logicalOr) {

            context.stack.head match {
              case RValue(x: Boolean, _) if x => Seq()
              case _ => Seq(bin.getOperand2, bin)
            }

          } else if (bin.getOperator == IASTBinaryExpression.op_logicalAnd) {

            context.stack.head match {
              case RValue(x: Boolean, _) if !x => Seq()
              case _ => Seq(bin.getOperand2, bin)
            }

          } else {
            Seq(bin.getOperand2, bin)
          }
        }
      case Gotoing =>
        Seq()
    }
  }
}