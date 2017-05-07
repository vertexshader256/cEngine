package app.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ ListBuffer }
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._
import org.eclipse.cdt.internal.core.dom.parser.c._

object Expressions {

  def parse(expr: IASTExpression, direction: Direction)(implicit context: State): Seq[IASTNode] = expr match {
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
          case AddressInfo(addr, _) => AddressInfo(addr, theType)
          case ValueInfo(value, _) =>
            val newAddr = context.allocateSpace(TypeHelper.sizeof(theType))
            context.setValue(TypeHelper.cast(theType, value).value, newAddr)
            AddressInfo(newAddr, theType)
        })

        Seq()
      }
    case fieldRef: IASTFieldReference =>
      if (direction == Entering) {
        Seq(fieldRef.getFieldOwner)
      } else {
        
        var baseAddr = -1
        
        val owner = context.stack.pop
        
        def resolve(theType: IType, addr: Int): CStructure = theType match {
          case typedef: CTypedef => 
            resolve(typedef.getType, addr)
          case struct: CStructure => struct
          case ptr: IPointerType => 
            baseAddr = context.readPtrVal(addr).value.asInstanceOf[Int]
            resolve(ptr.getType, baseAddr)
        }
        
        val structType = owner match {
          case AddressInfo(addr, theType) =>
            baseAddr = addr
            resolve(theType, addr)
        }
        
        var offset = 0
        var resultAddress: AddressInfo = null
        structType.getFields.foreach{field =>
          if (field.getName == fieldRef.getFieldName.getRawSignature) {
            // can assume names are unique
            resultAddress = AddressInfo(baseAddr + offset, field.getType)
          } else {
            offset += TypeHelper.sizeof(field.getType)
          }
        }

        context.stack.push(resultAddress)
             
        Seq()
      }
    case subscript: IASTArraySubscriptExpression =>
      if (direction == Entering) {
        Seq(subscript.getArgument, subscript.getArrayExpression)
      } else {
        
        if (!subscript.getArrayExpression.isInstanceOf[IASTArraySubscriptExpression]) {

          val arrayVarPtr = context.stack.pop.asInstanceOf[AddressInfo]
          
          val indexes = new ListBuffer[Int]()
          var itr: IASTNode = subscript
          while (itr.isInstanceOf[IASTArraySubscriptExpression]) {

            val result: Int = context.stack.pop match {
              case x @ ValueInfo(_, _) => TypeHelper.cast(TypeHelper.pointerType, x.value).value.asInstanceOf[Int]
              case info @ AddressInfo(_, _) =>
                info.value.value match {
                  case int: Int => int
                  case long: Long => long.toInt
                }
            }
            
            indexes += result
            itr = itr.getParent
          }

          var offset = arrayVarPtr.value.value.asInstanceOf[Int]
          var aType = arrayVarPtr.theType

          indexes.foreach{ index =>

            aType = context.resolve(aType)

            val step = TypeHelper.sizeof(aType)
            if (aType.isInstanceOf[IArrayType] || TypeHelper.resolve(aType).getKind == IBasicType.Kind.eChar && !aType.isInstanceOf[IBasicType]) {
              // special case for strings
              offset = context.readPtrVal(offset + index * step).value.asInstanceOf[Int]
            } else {
              offset += index * step
            }
          }

          context.stack.push(AddressInfo(offset, aType))
        }

        Seq()
      }
    case unary: IASTUnaryExpression =>
      import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression._

      def resolveVar(variable: Any): AddressInfo = {
        variable match {
          case info @ AddressInfo(addr, theType) => info
        }
      }

      def not(theVal: Any): AnyVal = theVal match {
        case info @ AddressInfo(_, _) => not(info.value)
        case ValueInfo(theVal, _) => not(theVal)
        case int: Int               => if (int == 0) 1 else 0
        case bool: Boolean => !bool
        case char: Character => if (char == 0) 1 else 0
      }
      
      if (direction == Entering) {
        Seq(unary.getOperand)
      } else {
        val one = ValueInfo(1, new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_UNSIGNED))

        unary.getOperator match {
          case `op_tilde` =>
            context.stack.push(ValueInfo2(~context.stack.pop.asInstanceOf[Int], null))
          case `op_not` => context.stack.push(ValueInfo2(not(context.stack.pop), one.theType))
          case `op_minus` =>
            context.stack.pop match {
              case ValueInfo(int: Int, theType)     => context.stack.push(ValueInfo2(-int, theType))
              case ValueInfo(doub: Double, theType)     => context.stack.push(ValueInfo2(-doub, theType))
              case info @ AddressInfo(_, _) =>
                val resolvedInfo = resolveVar(info)
              
                val basicType = resolvedInfo.theType.asInstanceOf[IBasicType]
                context.stack.push(basicType.getKind match {
                  case `eInt`    => ValueInfo2(-resolvedInfo.value.value.asInstanceOf[Int], basicType)
                  case `eDouble` => ValueInfo2(-resolvedInfo.value.value.asInstanceOf[Double], basicType)
                })
            }
          case `op_postFixIncr` =>
            val info = resolveVar(context.stack.pop)
            val newVal = BinaryExpr.evaluate(info.value, one, IASTBinaryExpression.op_plus)

            context.stack.push(info.value)
            context.setValue(newVal.value, info.address)
          case `op_postFixDecr` =>          
            val info = resolveVar(context.stack.pop)
            val newVal = BinaryExpr.evaluate(info.value, one, IASTBinaryExpression.op_minus)
            
            // push then set
            context.stack.push(info.value)
            context.setValue(newVal.value, info.address)
          case `op_prefixIncr` =>
            val info = resolveVar(context.stack.pop)
            val newVal = BinaryExpr.evaluate(info.value, one, IASTBinaryExpression.op_plus)
            
            // set then push
            context.setValue(newVal.value, info.address)
            context.stack.push(newVal)
          case `op_prefixDecr` =>
            val info = resolveVar(context.stack.pop)
            val newVal = BinaryExpr.evaluate(info.value, one, IASTBinaryExpression.op_minus)
            
            // set then push
            context.setValue(newVal.value, info.address)
            context.stack.push(newVal)
          case `op_sizeof` =>
            context.stack.push(context.stack.pop match {
              case info @ AddressInfo(_, theType) => ValueInfo2(info.sizeof, TypeHelper.pointerType)
            })
          case `op_amper` =>
            context.stack.pop match {
              case info @ AddressInfo(_, _) =>
                info.theType match {
                  case fcn: CFunctionType => context.stack.push(AddressInfo(info.address, fcn))
                  case x: IType => context.stack.push(ValueInfo(info.address, x))
                }
            }
          case `op_star` =>
            context.stack.pop match {
              case ValueInfo(int: Int, theType) =>
                context.stack.push(AddressInfo(int, TypeHelper.resolve(theType)))
              case info @ AddressInfo(_,_) =>
                val nestedType = info.theType match {
                  case ptr: IPointerType => ptr.getType
                  case array: IArrayType => array.getType
                }
                
                if (!nestedType.isInstanceOf[IFunctionType]) {
                  val value = info.value.value
                  context.stack.push(AddressInfo(value.asInstanceOf[Int], nestedType))
                } else {
                  // function pointers can ignore the star
                  context.stack.push(info)
                }

           }
          case `op_bracketedPrimary` => // not sure what this is for but I need it for weird stuff like (k*)++
        }
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
        context.stack.push(ValueInfo2(TypeHelper.sizeof(theType), TypeHelper.pointerType))
        Seq()
      }
    case call: IASTFunctionCallExpression =>
      if (direction == Exiting) {

        val name = context.stack.pop match {
          case x if context.hasFunction(call.getFunctionNameExpression.getRawSignature) =>
            call.getFunctionNameExpression.getRawSignature
          case AddressInfo(addr, theType: CFunctionType) =>
            context.getFunctionByIndex(addr).name
          case AddressInfo(addr, theType: CPointerType) => context.getFunctionByIndex(context.readPtrVal(addr).value.asInstanceOf[Int]).name
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
              BinaryExpr.parseAssign(bin.getOperator, op1, op2)
          } else {
            val op2 = context.stack.pop match {
              case info @ AddressInfo(_, _) => info.value
              case value @ ValueInfo(_, _) => value
            }

            val op1 = context.stack.pop match {
              case info @ AddressInfo(_, _) => info.value
              case value @ ValueInfo(_, _) => value
            }

            BinaryExpr.evaluate(op1, op2, bin.getOperator)
          }

          context.stack.push(result)
          Seq()
        } else {
          // short circuiting
          if (bin.getOperator == IASTBinaryExpression.op_logicalOr) {
            
            context.stack.head match {
              case ValueInfo(x: Boolean, _) if x => Seq()
              case _ => Seq(bin.getOperand2, bin)
            }

          } else if (bin.getOperator == IASTBinaryExpression.op_logicalAnd) {
            
            context.stack.head match {
              case ValueInfo(x: Boolean, _) if !x => Seq()
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