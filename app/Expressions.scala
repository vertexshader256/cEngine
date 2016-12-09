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
        val result = context.stack.pop.asInstanceOf[Boolean]

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
        
        context.stack.push(context.stack.pop match {
          case addy @ Address(_) => AddressInfo(addy, theType)
          case Variable(value, _) => AddressInfo(Address(value.value.asInstanceOf[Int]), theType)
          case lit @ Literal(str) => TypeHelper.cast(TypeHelper.resolve(theType), lit.cast.value)
          case int: Int => TypeHelper.cast(TypeHelper.resolve(theType), int)
        })

        Seq()
      }
    case fieldRef: IASTFieldReference =>
      if (direction == Entering) {
        Seq(fieldRef.getFieldOwner)
      } else {
        
        var baseAddr: Address = Address(-1)
        
        val owner = context.stack.pop
        
        val structType = if (fieldRef.isPointerDereference) {
          owner match {
            case Variable(value, info) => 
              baseAddr = Address(value.value.asInstanceOf[Int])
              info.theType.asInstanceOf[IPointerType].getType.asInstanceOf[CStructure]
            case AddressInfo(addr, theType) => 
              baseAddr = addr
              theType match {
                case struct: CStructure => struct
                case ptr: IPointerType => 
                  baseAddr = Address(context.readPtrVal(addr))
                  ptr.getType.asInstanceOf[CStructure]
              }
          }
        } else {
          owner match {
            case VarRef(id) =>
              val theVar = context.currentFunctionContext.resolveId(id)
              baseAddr = theVar.address
              theVar.theType.asInstanceOf[CStructure]
            case AddressInfo(addr, theType) => 
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
            case Variable(_, info) => info
            case addr @ AddressInfo(_, theType) => addr
          }
          
          val indexes = new ListBuffer[Int]()
          var itr: IASTNode = subscript
          while (itr.isInstanceOf[IASTArraySubscriptExpression]) {
            val result: Int = (context.stack.pop match {
              case Variable(value, _) =>
                value.value.asInstanceOf[Int]
              case lit @ Literal(_) => lit.cast.value.asInstanceOf[Int]
              case int: Int => int
              case double: Double => double.toInt
              case ValueInfo(x, _) => x.asInstanceOf[Int]
            })
            
            indexes += result
            itr = itr.getParent
          }
          
          val dimensions = new ListBuffer[Int]()
          var typeItr: IType = arrayVarPtr.theType
          while (typeItr.isInstanceOf[IArrayType]) {
            if (typeItr.asInstanceOf[IArrayType].hasSize &&
                typeItr.asInstanceOf[IArrayType].getSize.numericalValue != null) {
              dimensions += typeItr.asInstanceOf[IArrayType].getSize.numericalValue.toInt
            }
            typeItr = typeItr.asInstanceOf[IArrayType].getType
          }
          
          val arrayAddress = context.readPtrVal(arrayVarPtr.address)
  
          val ancestors = Utils.getAncestors(subscript)
          
          // We have to treat the destination op differently in an assignment
          val isParsingAssignmentDest = ancestors.find{ _.isInstanceOf[IASTBinaryExpression]}.map { binary =>
            val bin = binary.asInstanceOf[IASTBinaryExpression]
            Utils.isAssignment(bin.getOperator)
          }.getOrElse(false)
          
          var offset = arrayAddress

          var aType = context.resolve(arrayVarPtr.theType)
          
          indexes.foreach{ arrayIndex =>
            aType match {
              case array: IArrayType =>
                val step = 4
                offset = context.readPtrVal(Address(offset + arrayIndex * step))
                aType = array.getType
              case ptr: IPointerType =>
                val step = 4
                offset = context.readPtrVal(Address(offset + arrayIndex * step))
                aType = ptr.getType
              case basic: IBasicType =>
                val step = TypeHelper.sizeof(basic)
                offset += arrayIndex * step
            }
          
            if (aType.isInstanceOf[IQualifierType]) {
              aType = aType.asInstanceOf[IQualifierType].getType
            }
          }

          val elementAddress = Address(offset)
          
          if (isParsingAssignmentDest) {
            context.stack.push(AddressInfo(elementAddress, aType))
          } else {
            context.stack.push(context.readVal(elementAddress, aType))
          }
        }

        Seq()
      }
    case unary: IASTUnaryExpression =>
      import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression._

      def resolveVar(variable: Any): (ValueInfo, AddressInfo) = {
        variable match {
          case Variable(value, info) =>
            val theType = TypeHelper.resolve(info.theType)
            val currentVal = context.readVal(info.address, theType).value
            (ValueInfo(currentVal, info.theType), info)
          case AddressInfo(addy, theType) => 
            val resolved = TypeHelper.resolve(theType)
            val currentVal = context.readVal(addy, resolved).value
            (ValueInfo(currentVal, resolved), AddressInfo(addy, resolved))
        }
      }

      if (direction == Entering) {
        Seq(unary.getOperand)
      } else {
        unary.getOperator match {
          case `op_not` => context.stack.push(context.stack.pop match {
            case Variable(value, info) =>
              value.value match {
                case int: Int => if (int == 0) 1 else 0
              }
            case int: Int               => if (int == 0) 1 else 0
            case ValueInfo(int: Int, _) => if (int == 0) 1 else 0
            case bool: Boolean => !bool
          })
          case `op_minus` =>
            val resolveLit = context.stack.pop match {
              case lit @ Literal(_) => lit.cast
              case x            => x
            }

            resolveLit match {
              case int: Int     => context.stack.push(-int)
              case ValueInfo(int: Int, _)     => context.stack.push(-int)
              case ValueInfo(doub: Double, _) => context.stack.push(-doub)
              case Variable(value, info) =>
                val (currentVal, resolvedInfo) = resolveVar(info)
              
                val basicType = resolvedInfo.theType.asInstanceOf[IBasicType]
                context.stack.push(basicType.getKind match {
                  case `eInt`    => -currentVal.value.asInstanceOf[Int]
                  case `eDouble` => -currentVal.value.asInstanceOf[Double]
                })
            }
          case `op_postFixIncr` =>
            val (currentVal, info) = resolveVar(context.stack.pop)
            val newVal = BinaryExpr.evaluate(currentVal, 1, IASTBinaryExpression.op_plus)
            
            // push then set
            context.stack.push(currentVal)
            context.setValue(newVal, info.address)   
          case `op_postFixDecr` =>          
            val (currentVal, info) = resolveVar(context.stack.pop)
            val newVal = BinaryExpr.evaluate(currentVal, 1, IASTBinaryExpression.op_minus)
            
            // push then set
            context.stack.push(currentVal)
            context.setValue(newVal, info.address)  
          case `op_prefixIncr` =>
            val (currentVal, info) = resolveVar(context.stack.pop)            
            val newVal = BinaryExpr.evaluate(currentVal, 1, IASTBinaryExpression.op_plus)
            
            // set then push
            context.setValue(newVal, info.address)  
            context.stack.push(newVal)
          case `op_prefixDecr` =>
            val (currentVal, info) = resolveVar(context.stack.pop)
            val newVal = BinaryExpr.evaluate(currentVal, 1, IASTBinaryExpression.op_minus)
            
            // set then push
            context.setValue(newVal, info.address)  
            context.stack.push(newVal)
          case `op_sizeof` =>
            context.stack.push(context.stack.pop match {
              case VarRef(name) =>
                context.currentFunctionContext.resolveId(name).sizeof
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
              case VarRef(id) =>
                val theVar = context.currentFunctionContext.resolveId(id)
                context.stack.push(theVar.address)
            }
          case `op_star` =>
            context.stack.pop match {
              case ValueInfo(char: Character, _) =>
                val target = Utils.getUnaryTarget(unary).foreach { name =>
                  val ptr = context.currentFunctionContext.resolveId(name.getRawSignature)
                  context.stack.push(context.readVal(Address(char), TypeHelper.resolve(ptr.theType)))
                }
              case ValueInfo(int: Int, _) =>
                val target = Utils.getUnaryTarget(unary).foreach { name =>
                  val ptr = context.currentFunctionContext.resolveId(name.getRawSignature)
                  context.stack.push(context.readVal(Address(int), TypeHelper.resolve(ptr.theType)))
                }
              case Variable(value, info) =>       
                val nestedType = info.theType match {
                  case ptr: IPointerType => ptr.getType
                  case array: IArrayType => array.getType
                }
                
                val isNested = nestedType.isInstanceOf[IPointerType]
                
                val specialCase = unary.getParent.isInstanceOf[IASTUnaryExpression] &&
                      unary.getParent.asInstanceOf[IASTUnaryExpression].getOperator == op_bracketedPrimary // (k*)++
                
                context.stack.push(
                  if (Utils.isOnLeftSideOfAssignment(unary) || isNested || specialCase) { 
                    val deref = context.readPtrVal(info.address)
                    AddressInfo(Address(deref), nestedType)
                  } else {
                    context.readVal(Address(value.value.asInstanceOf[Int]), TypeHelper.resolve(info.theType))
                  }
                )
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
        if (context.currentFunctionContext.visited.contains(bin.getOperand2)) {
          val result = if (Utils.isAssignment(bin.getOperator)) {
              var op2: Any = context.stack.pop
              var op1: Any = context.stack.pop
              BinaryExpr.parseAssign(bin, bin.getOperator, op1, op2)
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