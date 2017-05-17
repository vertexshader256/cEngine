package scala.c_engine

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.annotation.switch

object Expressions {

  def parse(expr: IASTExpression, direction: Direction)(implicit context: State): Seq[IASTNode] = (expr: @switch) match {
    case exprList: IASTExpressionList =>
      if (direction == Entering) {
        exprList.getExpressions
      } else {
        Seq()
      }
    case ternary: IASTConditionalExpression =>
       if (direction == Entering) {
        Seq(ternary.getLogicalConditionExpression)
      } else {
        val result = TypeHelper.resolveBoolean(context.stack.pop)

        if (result) {
          Seq(ternary.getPositiveResultExpression)
        } else {
          Seq(ternary.getNegativeResultExpression)
        }
      }
    case cast: IASTCastExpression =>
      if (direction == Entering) {
        Seq(cast.getOperand, cast.getTypeId)
      } else {
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
    case fieldRef: IASTFieldReference =>
      if (direction == Entering) {
        Seq(fieldRef.getFieldOwner)
      } else {
        
        var baseAddr = -1
        
        val struct = context.stack.pop.asInstanceOf[LValue]
        
        def resolve(theType: IType, addr: Int): CStructure = theType match {
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
    case subscript: IASTArraySubscriptExpression =>
      if (direction == Entering) {
        Seq(subscript.getArrayExpression, subscript.getArgument)
      } else {

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
        
          val offset = aType match {
            case ptrVar: ArrayVariable =>
              aType = ptrVar.theType
              (arrayVarPtr.address + 4) + index * TypeHelper.sizeof(aType)
            case array: IArrayType =>
              aType = array.getType
              context.readPtrVal(arrayVarPtr.address) + index * TypeHelper.sizeof(aType)
            case ptr: IPointerType =>
              aType = ptr.getType
              context.readPtrVal(arrayVarPtr.address) + index * TypeHelper.sizeof(aType)
          }

        aType = TypeHelper.stripSyntheticTypeInfo(aType)

        context.stack.push(LValue(offset, aType))

        Seq()
      }
    case unary: IASTUnaryExpression =>
      if (direction == Entering) {
        Seq(unary.getOperand)
      } else {
        UnaryExpression.execute(unary)
        Seq()
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
    case call: IASTFunctionCallExpression =>
      if (direction == Exiting) {

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

        val arg = call.getArguments.map{x => context.stack.pop}

        val resolvedArgs = arg.map{x =>
          Utils.allocateString(x, false)
        }

        context.callTheFunction(name, call, resolvedArgs)

      } else {
        call.getArguments.reverse ++ Seq(call.getFunctionNameExpression)
      }
    case bin: IASTBinaryExpression =>
      if (direction == Exiting) {
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
      } else {
        Seq(bin.getOperand1)
      }
  }
}