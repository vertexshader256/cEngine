package c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.annotation.switch

object Expressions {

  def parse(expr: IASTExpression)(implicit context: State): PartialFunction[Direction, Seq[IASTNode]] = expr match {
    case exprList: IASTExpressionList => {
      case Stage2 => exprList.getExpressions
      case Exiting => Seq ()
    }
    case ternary: IASTConditionalExpression => {
      case Stage2 => Seq (ternary.getLogicalConditionExpression)
      case Exiting =>
        val result = TypeHelper.resolveBoolean (context.stack.pop)

        if (result) {
          Seq (ternary.getPositiveResultExpression)
        } else {
          Seq (ternary.getNegativeResultExpression)
        }
    }
    case cast: IASTCastExpression => {
      case Stage2 => Seq(cast.getOperand, cast.getTypeId)
      case Exiting =>
        val theType = context.stack.pop.asInstanceOf[TypeInfo].value
        val operand = context.stack.pop

        context.stack.push(operand match {
          case str @ StringLiteral(_) => str
          case LValue(addr, aType) =>
            theType match {
              case ptr: IPointerType if aType.isInstanceOf[IArrayType] =>
                val newAddr = context.allocateSpace(4)
                context.Stack.writeToMemory(addr, newAddr, theType)
                LValue(newAddr, theType)
              case _ => LValue(addr, theType)
            }
          case RValue(value, _) =>
            val newAddr = context.allocateSpace(TypeHelper.sizeof(theType))
            context.Stack.writeToMemory(TypeHelper.cast(theType, value).value, newAddr, theType)
            LValue(newAddr, theType)
        })

        Seq()
    }
    case fieldRef: IASTFieldReference => {
      case Stage2 => Seq(fieldRef.getFieldOwner)
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
    case subscript: IASTArraySubscriptExpression => {
      case Stage2 => Seq(subscript.getArrayExpression, subscript.getArgument)
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
    case unary: IASTUnaryExpression => {
      case Stage2 => Seq(unary.getOperand)
      case Exiting =>
        UnaryExpression.execute(unary)
        Seq()
      case Gotoing => Seq()
    }
    case lit: IASTLiteralExpression => {
      case Exiting =>
        //println("PUSHING LIT: " + castLiteral(lit))

        val litStr = lit.getRawSignature
        if (litStr.head == '\"' && litStr.last == '\"') {
          context.stack.push(StringLiteral(litStr))
        } else {
          context.stack.push(Literal.cast(lit.getRawSignature))
        }
        Seq()
    }
    case id: IASTIdExpression => {
      case Exiting =>
        context.stack.push(context.context.resolveId(id.getName).get)
        Seq()
    }
    case typeExpr: IASTTypeIdExpression => {
      // used for sizeof calls on a type
      case Stage2 =>
        Seq(typeExpr.getTypeId)
      case Exiting =>
        val theType = context.stack.pop.asInstanceOf[TypeInfo].value
        context.stack.push(RValue(TypeHelper.sizeof(theType), TypeHelper.pointerType))
        Seq()
    }
    case call: IASTFunctionCallExpression => {
      case Stage2 => call.getArguments.reverse ++ Seq(call.getFunctionNameExpression)
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
    case bin: IASTBinaryExpression => {
      case Stage1 => Seq(bin.getOperand1)
      case Stage2 =>
        (bin.getOperator, context.stack.head) match {
          case (IASTBinaryExpression.op_logicalOr, RValue(x: Boolean, _)) if x => context.context.pathStack.pop; Seq()
          case (IASTBinaryExpression.op_logicalAnd, RValue(x: Boolean, _)) if !x => context.context.pathStack.pop; Seq()
          case _ => Seq(bin.getOperand2)
        }
      case Exiting =>
        val op2 = context.stack.pop
        val op1 = context.stack.pop

        val result = if (Utils.isAssignment(bin.getOperator)) {
          BinaryExpr.parseAssign(bin, bin.getOperator, op1, op2)
        } else {
          BinaryExpr.evaluate(bin, op1, op2, bin.getOperator)
        }

        context.stack.push(result)
        Seq()
      case Gotoing =>
        Seq()
    }
  }
}