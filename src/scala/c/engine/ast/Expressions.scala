package c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.annotation.switch

object Expressions {

  def recurse(expr: IASTInitializerClause)(implicit state: State): Seq[ValueType] = expr match {
    case exprList: IASTExpressionList =>
      exprList.getExpressions.flatMap{x => recurse(x)}
    case ternary: IASTConditionalExpression =>
      val result = TypeHelper.resolveBoolean (recurse(ternary.getLogicalConditionExpression).head)

      if (result) {
        state.context.stack.push(recurse(ternary.getPositiveResultExpression).head)
      } else {
        state.context.stack.push(recurse(ternary.getNegativeResultExpression).head)
      }
      Seq()
    case cast: IASTCastExpression =>
        val theType = TypeHelper.getType(cast.getTypeId).theType
        val operand = recurse(cast.getOperand).head

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

        val struct = recurse(fieldRef.getFieldOwner).head.asInstanceOf[LValue]

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
      val arrayVarPtr = recurse(subscript.getArrayExpression).head.asInstanceOf[LValue]
      val index = recurse(subscript.getArgument).head match {
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
      Seq(UnaryExpression.execute(recurse(unary.getOperand).head, unary))
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
        val pop = recurse(call.getFunctionNameExpression).head

        val name = if (state.hasFunction(call.getFunctionNameExpression.getRawSignature)) {
          call.getFunctionNameExpression.getRawSignature
        } else {
          val info = pop.asInstanceOf[LValue]
          val resolved = TypeHelper.stripSyntheticTypeInfo(info.theType)
          resolved match {
            case ptr: IPointerType => state.getFunctionByIndex(info.value.value.asInstanceOf[Int]).name
          }
        }

        val args = call.getArguments.map{x => recurse(x).head}

        println(args.toList)
        state.callTheFunction(name, call, args).map{ x => Seq(x)}.getOrElse(Seq())
    case bin: IASTBinaryExpression =>
        val op2 = recurse(bin.getOperand2).head
        val op1 = recurse(bin.getOperand1).head

        val shouldShortCircuit = (bin.getOperator, op1) match {
          case (IASTBinaryExpression.op_logicalOr, RValue(x: Boolean, _)) if x => true
          case (IASTBinaryExpression.op_logicalAnd, RValue(x: Boolean, _)) if !x => true
          case _ => false
        }

        val result = if (shouldShortCircuit) {
          op1
        } else {
          if (Utils.isAssignment(bin.getOperator)) {
            BinaryExpr.parseAssign(bin, bin.getOperator, op1, op2)
          } else {
            BinaryExpr.evaluate(bin, op1, op2, bin.getOperator)
          }
        }

        Seq(result)
  }

  def parse(expr: IASTExpression)(implicit state: State): PartialFunction[Direction, Seq[IASTNode]] = expr match {
    case exprList: IASTExpressionList => {
      case Stage2 => exprList.getExpressions
      case Exiting => Seq ()
    }
    case ternary: IASTConditionalExpression => {
      case Exiting =>
        recurse(ternary)
        Seq()
    }
    case cast: IASTCastExpression => {
      case Exiting =>
        state.context.stack.push(recurse(cast).head)
        Seq()
    }
    case fieldRef: IASTFieldReference => {
      case Exiting =>

        state.context.stack.push(recurse(fieldRef).head)

        Seq()
    }
    case subscript: IASTArraySubscriptExpression => {
      case Exiting =>
        state.context.stack.push(recurse(subscript).head)
        Seq()
    }
    case unary: IASTUnaryExpression => {
      case Stage2 => Seq(unary.getOperand)
      case Exiting =>
        state.context.stack.push(UnaryExpression.execute(state.context.stack.pop, unary))
        Seq()
      case Gotoing => Seq()
    }
    case lit: IASTLiteralExpression => {
      case Exiting =>
        state.context.stack.push(recurse(lit).head)
        Seq()
    }
    case id: IASTIdExpression => {
      case Exiting =>
        state.context.stack.push(state.context.resolveId(id.getName).get)
        Seq()
    }
    case typeExpr: IASTTypeIdExpression => {
      // used for sizeof calls on a type
      case Exiting =>
        val theType = TypeHelper.getType(typeExpr.getTypeId).theType
        state.context.stack.push(RValue(TypeHelper.sizeof(theType), TypeHelper.pointerType))
        Seq()
    }
    case call: IASTFunctionCallExpression => {
      case Exiting =>
        val result = recurse(call)
        if (!result.isEmpty) {
          state.context.stack.push(result.head)
        }
        Seq()
      case Gotoing => Seq()
    }
    case bin: IASTBinaryExpression => {
      case Stage1 => Seq(bin.getOperand1)
      case Stage2 =>
        (bin.getOperator, state.context.stack.head) match {
          case (IASTBinaryExpression.op_logicalOr, RValue(x: Boolean, _)) if x => state.context.pathStack.pop; Seq()
          case (IASTBinaryExpression.op_logicalAnd, RValue(x: Boolean, _)) if !x => state.context.pathStack.pop; Seq()
          case _ => Seq(bin.getOperand2)
        }
      case Exiting =>
        val op2 = state.context.stack.pop
        val op1 = state.context.stack.pop

        val result = if (Utils.isAssignment(bin.getOperator)) {
          BinaryExpr.parseAssign(bin, bin.getOperator, op1, op2)
        } else {
          BinaryExpr.evaluate(bin, op1, op2, bin.getOperator)
        }

        state.context.stack.push(result)
        Seq()
      case Gotoing =>
        Seq()
    }
  }
}