package app.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ ListBuffer, Stack }
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;
import java.math.BigInteger
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._
import org.eclipse.cdt.internal.core.dom.parser.c._
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._
import Variable._

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
        val theType = context.stack.pop.asInstanceOf[IType]
        val operand = context.stack.pop

        context.stack.push(operand match {
          case addy @ Address(_) => ValueInfo(addy, theType)
          case Addressable(addr) => addr
          case ValueInfo(value, _) => TypeHelper.cast(theType, value)
          case lit @ Literal(str) => TypeHelper.cast(theType, lit.cast.value)
          case int: Int => TypeHelper.cast(theType, int)
          case long: Long => TypeHelper.cast(theType, long)
          case double: Double => TypeHelper.cast(theType, double)
          case float: Float => TypeHelper.cast(theType, float)
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
            baseAddr = Address(context.readPtrVal(addr))
            resolve(ptr.getType, baseAddr)
        }
        
        val structType = if (fieldRef.isPointerDereference) {

          owner match {
            case Addressable(AddressInfo(addr, theType)) =>
              baseAddr = addr            
              resolve(theType, addr)
          }
        } else {
          owner match {
            case Addressable(AddressInfo(addr, theType)) =>
              baseAddr = addr
              theType match {
                case typedef: CTypedef => typedef.getType.asInstanceOf[CStructure]
                case struct: CStructure => struct
              }
          }
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
            case Addressable(info) => info
          }
          
          val indexes = new ListBuffer[Int]()
          var itr: IASTNode = subscript
          while (itr.isInstanceOf[IASTArraySubscriptExpression]) {
            val result: Int = (context.stack.pop match {
              case lit @ Literal(_) => lit.cast.value.asInstanceOf[Int]
              case int: Int => int
              case long: Long => long.toInt
              case double: Double => double.toInt
              case ValueInfo(x, _) => x.asInstanceOf[Int]
              case Addressable(AddressInfo(addr, theType)) =>
                context.readVal(addr, theType).value match {
                  case int: Int => int
                  case char: Character => char.toInt
                  case long: Long => long.toInt
                }
            })
            
            indexes += result
            itr = itr.getParent
          }
          
          val arrayAddress = context.readPtrVal(arrayVarPtr.address)
  
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
              offset = context.readPtrVal(Address(offset + arrayIndex * step))
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

      def resolveVar(variable: Any): (ValueInfo, AddressInfo) = {
        variable match {
          case Addressable(AddressInfo(addr, theType)) =>
            val currentVal = context.readVal(addr, theType).value
            (ValueInfo(currentVal, theType), AddressInfo(addr, theType))
        }
      }

      def not(theVal: Any): AnyVal = theVal match {
        case Addressable(AddressInfo(addr, theType)) => not(context.readVal(addr, theType).value)
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
          case `op_tilde` => context.stack.push(~context.stack.pop.asInstanceOf[Int])
          case `op_not` => context.stack.push(not(context.stack.pop))
          case `op_minus` =>
            val resolveLit = context.stack.pop match {
              case lit @ Literal(_) => lit.cast
              case x            => x
            }

            resolveLit match {
              case int: Int     => context.stack.push(-int)
              case ValueInfo(int: Int, _)     => context.stack.push(-int)
              case ValueInfo(doub: Double, _) => context.stack.push(-doub)
              case Variable(info) =>
                val (currentVal, resolvedInfo) = resolveVar(info)
              
                val basicType = resolvedInfo.theType.asInstanceOf[IBasicType]
                context.stack.push(basicType.getKind match {
                  case `eInt`    => -currentVal.value.asInstanceOf[Int]
                  case `eDouble` => -currentVal.value.asInstanceOf[Double]
                })
            }
          case `op_postFixIncr` =>
            val vari = context.stack.pop
            val (currentVal, info) = resolveVar(vari)
            val newVal = BinaryExpr.evaluate(currentVal, one, IASTBinaryExpression.op_plus)
            
            if (Utils.isOnLeftSideOfAssignment(unary) && unary.getParent.isInstanceOf[IASTUnaryExpression] && unary.getParent.asInstanceOf[IASTUnaryExpression].getOperator == op_star) {
              context.stack.push(vari)
            } else {
              // push then set
              context.stack.push(currentVal)
              context.setValue(newVal, info.address) 
            }
          case `op_postFixDecr` =>          
            val (currentVal, info) = resolveVar(context.stack.pop)
            val newVal = BinaryExpr.evaluate(currentVal, one, IASTBinaryExpression.op_minus)
            
            // push then set
            context.stack.push(currentVal)
            context.setValue(newVal, info.address)  
          case `op_prefixIncr` =>
            val (currentVal, info) = resolveVar(context.stack.pop)            
            val newVal = BinaryExpr.evaluate(currentVal, one, IASTBinaryExpression.op_plus)
            
            // set then push
            context.setValue(newVal, info.address)  
            context.stack.push(newVal)
          case `op_prefixDecr` =>
            val (currentVal, info) = resolveVar(context.stack.pop)
            val newVal = BinaryExpr.evaluate(currentVal, one, IASTBinaryExpression.op_minus)
            
            // set then push
            context.setValue(newVal, info.address)  
            context.stack.push(newVal)
          case `op_sizeof` =>
            context.stack.push(context.stack.pop match {
              case Variable(info) =>
                info.sizeof
              case ValueInfo(_, theType) => TypeHelper.sizeof(theType)
              case AddressInfo(_, theType) => TypeHelper.sizeof(theType)
              case _: Character => 1
              case _: Int => 4
              case _: Short => 2
              case _: Long => 8
              case _: Float => 4
              case _: Double => 8
            })
          case `op_amper` =>
            context.stack.pop match {
              case Addressable(info) =>
                info.theType match {
                  case fcn: CFunctionType => context.stack.push(context.readPtrVal(info.address))
                  case _ => context.stack.push(info.address)
                }
            }
          case `op_star` =>
            context.stack.pop match {
              case ValueInfo(int: Int, theType) =>
                context.stack.push(context.readVal(Address(int), TypeHelper.resolve(theType)))
              case ValueInfo(addr @ Address(_), theType) =>
                val theVal = context.readVal(addr, TypeHelper.resolve(theType))
                context.stack.push(theVal)
              case theVar @ Variable(info) =>
                val nestedType = info.theType match {
                  case ptr: IPointerType => ptr.getType
                  case array: IArrayType => array.getType
                }
                
                if (!nestedType.isInstanceOf[IFunctionType]) {
                  
                  val isNested = nestedType.isInstanceOf[IPointerType]
                  
                  val specialCase = unary.getParent.isInstanceOf[IASTUnaryExpression] &&
                        unary.getParent.asInstanceOf[IASTUnaryExpression].getOperator == op_bracketedPrimary // (k*)++
                  
                  context.stack.push(
                    if (Utils.isOnLeftSideOfAssignment(unary) || isNested || specialCase) { 
                      val deref = context.readPtrVal(info.address)
                      
                       if (Utils.isOnLeftSideOfAssignment(unary) && unary.getChildren.size == 1 && unary.getChildren.head.isInstanceOf[IASTUnaryExpression] && 
                          unary.getChildren.head.asInstanceOf[IASTUnaryExpression].getOperator == op_postFixIncr) {
                        context.setValue(info.value.value.asInstanceOf[Int] + 1, info.address)
                      }
                      
                      AddressInfo(Address(deref), nestedType)
                    } else {
                      context.readVal(Address(info.value.value.asInstanceOf[Int]), TypeHelper.resolve(info.theType))
                    }
                  )
                } else {
                  // function pointers can ignore the star
                  context.stack.push(theVar)
                }
              case address @ AddressInfo(addr, theType) =>
                theType match {
                  
                  case ptr: IPointerType =>
                    // nested pointers
                    val deref = context.readPtrVal(addr)
                    val refAddressInfo = AddressInfo(Address(deref), ptr.getType)
                    context.stack.push(refAddressInfo)
                  case basic: IBasicType =>
                    // actually dereference
                    context.stack.push(context.readVal(addr, basic))
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
          context.stack.push(Literal(lit.getRawSignature))
        }
      }
      Seq()
    case id: IASTIdExpression =>
      if (direction == Exiting) {
        //println("PUSHING ID: " + id.getName.getRawSignature)
        context.stack.push(VarRef(id.getName.getRawSignature))
      }
      Seq()
    case typeExpr: IASTTypeIdExpression =>
      // used for sizeof calls on a type
      if (direction == Entering) {
        Seq(typeExpr.getTypeId)
      } else {
        val theType = context.stack.pop.asInstanceOf[IType]
        context.stack.push(TypeHelper.sizeof(theType))
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
              case bool: Boolean if bool => Seq()
              case _ => Seq(bin.getOperand2, bin)
            }

          } else if (bin.getOperator == IASTBinaryExpression.op_logicalAnd) {
            
            context.stack.head match {
              case bool: Boolean if !bool => Seq()
              case int: Int if int == 0 => Seq()
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