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
          case info @ AddressInfo(_, _) => info
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
        
        var baseAddr: Address = Address(-1)
        
        val owner = context.stack.pop
        
        def resolve(theType: IType, addr: Address): CStructure = theType match {
          case typedef: CTypedef => 
            resolve(typedef.getType, addr)
          case struct: CStructure => struct
          case ptr: IPointerType => 
            baseAddr = Address(context.readPtrVal(addr).value.asInstanceOf[Int])
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

          val arrayVarPtr: AddressInfo = context.stack.pop match {
            case info @ AddressInfo(_, _) => info
          }
          
          val indexes = new ListBuffer[Int]()
          var itr: IASTNode = subscript
          while (itr.isInstanceOf[IASTArraySubscriptExpression]) {

            val first = (context.stack.pop match {
              case ValueInfo(x, _) => x
              case x => x
            })

            val result: Int = (first match {
              case int: Int => int
              case long: Long => long.toInt
              case double: Double => double.toInt
              case info @ AddressInfo(addr, theType) =>
                info.value.value match {
                  case int: Int => int
                  case char: Character => char.toInt
                  case long: Long => long.toInt
                }
            })
            
            indexes += result
            itr = itr.getParent
          }
          
          val arrayAddress = context.readPtrVal(arrayVarPtr.address).value.asInstanceOf[Int]
  
          val ancestors = Utils.getAncestors(subscript)

          var offset = arrayAddress
          
          var arrayTypes = new ListBuffer[IType]();
          
          var tempType = arrayVarPtr.theType
          
          while (!tempType.isInstanceOf[IBasicType]) {
            tempType = context.resolve(tempType)
            arrayTypes += tempType
          }

          val indexTypes = indexes zip arrayTypes

          indexTypes.foreach{ case(arrayIndex, aType) =>
            val step = TypeHelper.sizeof(aType)
            if (aType.isInstanceOf[IArrayType] || TypeHelper.resolve(aType).getKind == IBasicType.Kind.eChar && !aType.isInstanceOf[IBasicType]) {
              // special case for strings
              offset = context.readPtrVal(Address(offset + arrayIndex * step)).value.asInstanceOf[Int]
            } else {
              offset += arrayIndex * step
            }
          }

          val elementAddress = Address(offset)

          context.stack.push(AddressInfo(elementAddress, indexTypes.last._2))
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
            context.stack.push(ValueInfo(~context.stack.pop.asInstanceOf[Int], null))
          case `op_not` => context.stack.push(ValueInfo(not(context.stack.pop), null))
          case `op_minus` =>
            context.stack.pop match {
              case ValueInfo(int: Int, theType)     => context.stack.push(ValueInfo(-int, theType))
              case ValueInfo(doub: Double, theType)     => context.stack.push(ValueInfo(-doub, theType))
              case Variable(info) =>
                val resolvedInfo = resolveVar(info)
              
                val basicType = resolvedInfo.theType.asInstanceOf[IBasicType]
                context.stack.push(basicType.getKind match {
                  case `eInt`    => ValueInfo(-resolvedInfo.value.value.asInstanceOf[Int], null)
                  case `eDouble` => ValueInfo(-resolvedInfo.value.value.asInstanceOf[Double], null)
                })
            }
          case `op_postFixIncr` =>
            val vari = context.stack.pop
            val info = resolveVar(vari)
            val newVal = BinaryExpr.evaluate(info.value, one, IASTBinaryExpression.op_plus)
            
            if (Utils.isOnLeftSideOfAssignment(unary) && unary.getParent.isInstanceOf[IASTUnaryExpression] && unary.getParent.asInstanceOf[IASTUnaryExpression].getOperator == op_star) {
              context.stack.push(vari)
            } else {
              // push then set
              context.stack.push(info.value)
              context.setValue(newVal.value, info.address)
            }
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
              case ValueInfo(_, theType) => ValueInfo(TypeHelper.sizeof(theType), null)
              case info @ AddressInfo(_, _) => ValueInfo(info.sizeof, null)
            })
          case `op_amper` =>
            context.stack.pop match {
              case info @ AddressInfo(_, _) =>
                info.theType match {
                  case fcn: CFunctionType => context.stack.push(context.readPtrVal(info.address))
                  case _ => context.stack.push(ValueInfo(info.address.value, new CBasicType(IBasicType.Kind.eInt, 0)))
                }
            }
          case `op_star` =>
            context.stack.pop match {
              case ValueInfo(int: Int, theType) =>
                context.stack.push(context.readVal(Address(int), TypeHelper.resolve(theType)))
              case ValueInfo(addr @ Address(_), theType) =>
                val theVal = context.readVal(addr, TypeHelper.resolve(theType))
                context.stack.push(theVal)
              case info @ AddressInfo(_,_) =>
                val nestedType = info.theType match {
                  case ptr: IPointerType => ptr.getType
                  case array: IArrayType => array.getType
                }
                
                if (!nestedType.isInstanceOf[IFunctionType]) {
                  
                  val isNested = nestedType.isInstanceOf[IPointerType]
                  
                  val specialCase = unary.getParent.isInstanceOf[IASTUnaryExpression] &&
                        unary.getParent.asInstanceOf[IASTUnaryExpression].getOperator == op_bracketedPrimary // (k*)++

                  val value = info.value.value
                  
                  context.stack.push(
                    if (Utils.isOnLeftSideOfAssignment(unary) || isNested || specialCase) { 
                      val deref = context.readPtrVal(info.address).value.asInstanceOf[Int]
                      
                       if (Utils.isOnLeftSideOfAssignment(unary) && unary.getChildren.size == 1 && unary.getChildren.head.isInstanceOf[IASTUnaryExpression] && 
                          unary.getChildren.head.asInstanceOf[IASTUnaryExpression].getOperator == op_postFixIncr) {
                        context.setValue(value.asInstanceOf[Int] + 1, info.address)
                      }
                      
                      AddressInfo(Address(deref), nestedType)
                    } else {
                      context.readVal(Address(value.asInstanceOf[Int]), TypeHelper.resolve(info.theType))
                    }
                  )
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
        //println("PUSHING ID: " + id.getName.getRawSignature
        context.stack.push(context.context.resolveId(id.getName.getRawSignature))
      }
      Seq()
    case typeExpr: IASTTypeIdExpression =>
      // used for sizeof calls on a type
      if (direction == Entering) {
        Seq(typeExpr.getTypeId)
      } else {
        val theType = context.stack.pop.asInstanceOf[TypeInfo].value
        context.stack.push(ValueInfo(TypeHelper.sizeof(theType), null))
        Seq()
      }
    case call: IASTFunctionCallExpression =>
      FunctionCallExpr.parse(call, direction)
    case bin: IASTBinaryExpression =>
      if (direction == Exiting) {
        if (context.context.visited.contains(bin.getOperand2)) {
          val result = if (Utils.isAssignment(bin.getOperator)) {
              val op2: Any = context.stack.pop
              val op1: Any = context.stack.pop
              BinaryExpr.parseAssign(bin.getOperator, op1, op2)
          } else {    
            BinaryExpr.parse(bin)
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