package app.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;

object Expressions {
  
  private def castLiteral(lit: IASTLiteralExpression): Any = {
    
    def isLongNumber(s: String): Boolean = (allCatch opt s.toLong).isDefined
    def isDoubleNumber(s: String): Boolean = (allCatch opt s.toDouble).isDefined
    
    val string = lit.getRawSignature
    
    if (string.head == '\"' && string.last == '\"') {
      string
    } else if (isLongNumber(string)) {
      string.toInt
    } else {
      string.toDouble
    }
  }
  
  
  
  def parse(expr: IASTExpression, direction: Direction, context: IASTContext): Seq[IASTNode] = expr match {
    case subscript: IASTArraySubscriptExpression =>
      if (direction == Entering) {
        Seq(subscript.getArrayExpression, subscript.getArgument)
      } else {

          // index is first on the stack
          val index = context.stack.pop match {
            case VarRef(indexVarName) => 
               context.vars.resolveId(indexVarName).value.asInstanceOf[Int]
            case x: Int => 
              x 
          }
          
          val name = context.stack.pop
          val arrayVar = context.vars.resolveId(name.asInstanceOf[VarRef].name)
          context.stack.push(Address(arrayVar.address.address + index * TypeHelper.sizeof(arrayVar.typeName), arrayVar.typeName))

          Seq()
      }
    case unary: IASTUnaryExpression =>
      import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression._
      
      def resolveVar(variable: Any, func: (Int) => Unit) = {
        variable match {
          case VarRef(name) =>
            val variable = context.vars.resolveId(name)
            
            if (variable.isPointer) {
              func(variable.refAddress.address)
            } else {
              func(variable.address.address)
            }
          case addy @ Address(_,_) => func(addy.address)
        }
      }
      
      if (direction == Entering) {
        Seq(unary.getOperand)
      } else {
        unary.getOperator match {
          case `op_minus` =>  
            
            def negativeResolver(variable: Variable) = resolveVar(variable.address, (address) => {
                context.stack.push(variable.typeName match {
                  case "int" => -Variable.data.getInt(address)
                  case "double" => -Variable.data.getDouble(address)
                })
              })
            
           context.stack.pop match {
              case int: Int => context.stack.push(-int)
              case doub: Double => context.stack.push(-doub)
              //case variable @ Variable(_) => negativeResolver(variable)
              case VarRef(name) => 
                val variable = context.vars.resolveId(name)
                negativeResolver(variable)
            }
          case `op_postFixIncr` =>     
            resolveVar(context.stack.pop, (address) => {
              context.stack.push(Variable.data.getInt(address))
              Variable.data.putInt(address, Variable.data.getInt(address) + 1)
            })
          case `op_postFixDecr` =>
            resolveVar(context.stack.pop, (address) => {
              context.stack.push(Variable.data.getInt(address))
              Variable.data.putInt(address, Variable.data.getInt(address) - 1)
            })
          case `op_prefixIncr` =>  
            resolveVar(context.stack.pop, (address) => {
              Variable.data.putInt(address, Variable.data.getInt(address) + 1)
              context.stack.push(Variable.data.getInt(address))
            })
         case `op_prefixDecr` =>  
           resolveVar(context.stack.pop, (address) => {
              Variable.data.putInt(address, Variable.data.getInt(address) - 1)
              context.stack.push(Variable.data.getInt(address))
            })
          case `op_sizeof` =>
            context.stack.pop match {
              case VarRef(name) =>
                context.stack.push(context.vars.resolveId(name).sizeof)
            }
          case `op_amper` =>
            context.stack.pop match {
              case VarRef(name) =>
                context.stack.push(context.vars.resolveId(name).address)
            }
          case `op_star` =>
            
            context.stack.pop match {
              case VarRef(varName) =>
                val refAddress = context.vars.resolveId(varName).refAddress
                context.stack.push(refAddress)
              case int: Int => int
            }
          case `op_bracketedPrimary` => // not sure what this is for
        }
        Seq()
      }
    case lit: IASTLiteralExpression =>
      if (direction == Exiting) {
        //println("PUSHING LIT: " + castLiteral(lit))
        if (context.currentType == null) {
          context.stack.push(castLiteral(lit))
        } else {
          context.currentType.getRawSignature match {
            case "double" => context.stack.push(lit.getRawSignature.toDouble)
            case "int" => context.stack.push(lit.getRawSignature.toInt)
            case "float" => context.stack.push(lit.getRawSignature.toFloat)
            case _ => context.stack.push(castLiteral(lit))
          }
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
        context.stack.push(TypeHelper.sizeof(context.stack.pop.asInstanceOf[String]))
        Seq()
      }
    case call: IASTFunctionCallExpression =>
      FunctionCallExpr.parse(call, direction, context)
      // only evaluate after leaving
      

    case bin: IASTBinaryExpression =>
      if (direction == Exiting) {
        
        val result = bin.getOperator match {
          case IASTBinaryExpression.op_assign => 
            var op2: Any = context.stack.pop
            var op1: Any = context.stack.pop
            BinaryExpr.parseAssign(op1, op2, context)
          case _ => BinaryExpr.parse(bin, context)
        }
        
        if (result != null) {
          context.stack.push(result)
        }
        Seq()
      } else {
        Seq(bin.getOperand1, bin.getOperand2)
      }
  }
}