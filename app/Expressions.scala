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

  def parse(expr: IASTExpression, direction: Direction, context: State, stack: State): Seq[IASTNode] = expr match {
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
          case VarRef(id) => AddressInfo(Address(stack.vars.resolveId(id).value.asInstanceOf[Int]), theType)
          case lit @ Literal(str) => TypeHelper.coerece(TypeHelper.resolve(theType), lit.cast)
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
            case VarRef(name) => 
              val struct = context.vars.resolveId(name)
              baseAddr = Address(struct.value.asInstanceOf[Int])
              struct.theType.asInstanceOf[IPointerType].getType.asInstanceOf[CStructure]
            case AddressInfo(addr, theType) => 
              baseAddr = addr
              theType match {
                case struct: CStructure => struct
                case ptr: IPointerType => 
                  baseAddr = Address(context.readVal(addr.value, TypeHelper.pointerType).asInstanceOf[Int])
                  ptr.getType.asInstanceOf[CStructure]
              }
          }
        } else {
          owner match {
            case VarRef(name) => 
              val struct = context.vars.resolveId(name)
              baseAddr = struct.address
              struct.theType.asInstanceOf[CStructure]
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
            case VarRef(name) => 
              val variable = context.vars.resolveId(name)
              AddressInfo(variable.address, variable.theType)
            case addr @ AddressInfo(_, theType) => addr
          }
          
          val indexes = new ListBuffer[Int]()
          var itr: IASTNode = subscript
          while (itr.isInstanceOf[IASTArraySubscriptExpression]) {
            indexes += (context.stack.pop match {
              case VarRef(indexVarName) =>
                context.vars.resolveId(indexVarName).value
              case lit @ Literal(_) => lit.cast
              case int: Int => int
              case double: Double => double.toInt
            }).asInstanceOf[Int]
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
          
          val arrayAddress = stack.readVal(arrayVarPtr.address.value, TypeHelper.pointerType).asInstanceOf[Int]
  
          val ancestors = Utils.getAncestors(subscript)
          
          // We have to treat the destination op differently in an assignment
          val isParsingAssignmentDest = ancestors.find{ _.isInstanceOf[IASTBinaryExpression]}.map { binary =>
            val bin = binary.asInstanceOf[IASTBinaryExpression]
            Utils.isAssignment(bin.getOperator)
          }.getOrElse(false)
          
          var offset = 0

          indexes.zipWithIndex.foreach{ case (arrayIndex, index) =>
            offset += arrayIndex * dimensions.slice(0, index).reduceOption{_ * _}.getOrElse(1)
          }

          val elementAddress = Address(arrayAddress) + offset * TypeHelper.sizeof(arrayVarPtr.theType)
  
          if (isParsingAssignmentDest) {
            context.stack.push(AddressInfo(elementAddress, TypeHelper.resolve(arrayVarPtr.theType)))
          } else {
            context.stack.push(stack.readVal(elementAddress.value, TypeHelper.resolve(arrayVarPtr.theType)))
          }
        }

        Seq()
      }
    case unary: IASTUnaryExpression =>
      import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression._

      def resolveVar(variable: Any, func: (Int, IBasicType) => Unit) = {
        variable match {
          case VarRef(name) =>
            val variable = context.vars.resolveId(name)
            func(variable.address.value, TypeHelper.resolve(variable.theType))
          case AddressInfo(addy, theType) => func(addy.value, TypeHelper.resolve(theType))
          case int: Int          => int
        }
      }

      if (direction == Entering) {
        Seq(unary.getOperand)
      } else {
        unary.getOperator match {
          case `op_not` => context.stack.pop match {
            case VarRef(id) =>
              stack.vars.resolveId(id).value match {
                case int: Int => context.stack.push(if (int == 0) 1 else 0)
              }
            case int: Int => context.stack.push(if (int == 0) 1 else 0)
            case bool: Boolean => context.stack.push(!bool)
          }
          case `op_minus` =>

            
            def negativeResolver(variable: RuntimeVariable) = {
              
              val info = AddressInfo(variable.address, variable.theType)
              
              resolveVar(info, (address, theType) => {
                val basicType = theType.asInstanceOf[IBasicType]
                context.stack.push(basicType.getKind match {
                  case `eInt`    => -stack.readVal(address, basicType).asInstanceOf[Int]
                  case `eDouble` => -stack.readVal(address, basicType).asInstanceOf[Double]
                })
            })}

            val cast = context.stack.pop match {
              case lit @ Literal(_) => lit.cast
              case x            => x
            }

            cast match {
              case int: Int     => context.stack.push(-int)
              case doub: Double => context.stack.push(-doub)
              //case variable @ Variable(_) => negativeResolver(variable)
              case VarRef(name) =>
                val variable = context.vars.resolveId(name)
                negativeResolver(variable)
            }
          case `op_postFixIncr` =>
            resolveVar(context.stack.pop, (address, theType) => {
              val currentVal = stack.readVal(address, theType)
              context.stack.push(currentVal)
              val info = AddressInfo(Address(address), theType)
              
              currentVal match {
                case int: Int => stack.setValue(int + 1, info)
                case char: Char => stack.setValue(char + 1, info)
                case float: Float => stack.setValue(float + 1.0, info)
                case double: Double => stack.setValue(double + 1.0, info)
                case short: Short => stack.setValue(short + 1, info)
              }
            })
          case `op_postFixDecr` =>
            resolveVar(context.stack.pop, (address, theType) => {
              val currentVal = stack.readVal(address, theType)
              context.stack.push(currentVal)
              val info = AddressInfo(Address(address), theType)
              
              currentVal match {
                case int: Int => stack.setValue(int - 1, info)
                case char: Char => stack.setValue(char - 1, info)
                case float: Float => stack.setValue(float - 1.0, info)
                case double: Double => stack.setValue(double - 1.0, info)
                case short: Short => stack.setValue(short - 1, info)
              }
              
            })
          case `op_prefixIncr` =>
            resolveVar(context.stack.pop, (address, theType) => {
              val currentVal = stack.readVal(address, theType)
              val info = AddressInfo(Address(address), theType)
              
              currentVal match {
                case int: Int => stack.setValue(int + 1, info)
                case char: Char => stack.setValue(char + 1, info)
                case float: Float => stack.setValue(float + 1.0, info)
                case double: Double => stack.setValue(double + 1.0, info)
                case short: Short => stack.setValue(short + 1, info)
              }
              
              context.stack.push(stack.readVal(address, theType))
            })
          case `op_prefixDecr` =>
            resolveVar(context.stack.pop, (address, theType) => {
              val currentVal = stack.readVal(address, theType)
              val info = AddressInfo(Address(address), theType)

              currentVal match {
                case int: Int => stack.setValue(int - 1, info)
                case char: Char => stack.setValue(char - 1, info)
                case float: Float => stack.setValue(float - 1.0, info)
                case double: Double => stack.setValue(double - 1.0, info)
                case short: Short => stack.setValue(short - 1, info)
              }
              
              context.stack.push(stack.readVal(address, theType))
            })
          case `op_sizeof` =>
            context.stack.pop match {
              case VarRef(name) =>
                context.stack.push(context.vars.resolveId(name).sizeof)
              case char: Char => context.stack.push(1)
              case int: Int => context.stack.push(4)
              case short: Short => context.stack.push(2)
              case long: Long => context.stack.push(8)
              case float: Float => context.stack.push(4)
              case double: Double => context.stack.push(8)
            }
          case `op_amper` =>
            context.stack.pop match {
              case VarRef(name) =>
                val variable = context.vars.resolveId(name)
                context.stack.push(variable.address)
            }
          case `op_star` =>
            context.stack.pop match {
              case VarRef(varName) =>       
                val ptr = context.vars.resolveId(varName)
                val ptrType = ptr.theType.asInstanceOf[IPointerType]
                val isNested = ptrType.getType.isInstanceOf[IPointerType]
                
                val specialCase = unary.getParent.isInstanceOf[IASTUnaryExpression] &&
                      unary.getParent.asInstanceOf[IASTUnaryExpression].getOperator == op_bracketedPrimary // (k*)++
                
                context.stack.push(
                  if (Utils.isOnLeftSideOfAssignment(unary) || isNested || specialCase) { 
                    val deref = stack.readVal(ptr.address.value, TypeHelper.pointerType).asInstanceOf[Int]
                    AddressInfo(Address(deref), ptrType.getType)
                  } else {
                    stack.readVal(ptr.value.asInstanceOf[Int], TypeHelper.resolve(ptr.theType))
                  }
                )
              case address @ AddressInfo(addr, theType) =>
                theType match {
                  
                  case ptr: IPointerType =>
                    // nested pointers
                    val deref = stack.readVal(addr.value, TypeHelper.pointerType).asInstanceOf[Int]
                    val refAddressInfo = AddressInfo(Address(deref), ptr.getType)
                    context.stack.push(refAddressInfo)
                  case basic: IBasicType =>
                    // actually dereference
                    context.stack.push(stack.readVal(addr.value, basic))
               }
           }
          case `op_bracketedPrimary` => // not sure what this is for but I need it for weird stuff like (k*)++
        }
        Seq()
      }
    case lit: IASTLiteralExpression =>
      if (direction == Exiting) {
        //println("PUSHING LIT: " + castLiteral(lit))

        context.stack.push(Literal(lit.getRawSignature))

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
      FunctionCallExpr.parse(call, direction, context)
    case bin: IASTBinaryExpression =>
      if (direction == Exiting) {
        if (context.vars.visited.contains(bin.getOperand2)) {
          val result = if (Utils.isAssignment(bin.getOperator)) {
              var op2: Any = context.stack.pop
              var op1: Any = context.stack.pop
              BinaryExpr.parseAssign(bin, bin.getOperator, op1, op2, context)
          } else {    
            BinaryExpr.parse(bin, context)
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