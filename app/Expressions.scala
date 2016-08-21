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
  
  def printf(context: IASTContext, args: Seq[Object]) = {
    val formatString = args.head.asInstanceOf[String].replaceAll("^\"|\"$", "")
 
    val buffer = new StringBuffer();
    val formatter = new Formatter(buffer, Locale.US);
    
    val resolvedStrings = args.tail.map{ _ match {
      case str: String => str.split("""\"""").mkString
      case x => x 
    }}.toArray
    
    formatter.format(formatString, resolvedStrings: _*)
    
    context.stdout ++= buffer.toString.split("""\\n""")
  }
  
  def parse(expr: IASTExpression, direction: Direction, context: IASTContext): Seq[IASTNode] = expr match {
    case subscript: IASTArraySubscriptExpression =>
      if (direction == Entering) {
        Seq(subscript.getArrayExpression, subscript.getArgument)
      } else {
          val inputs = (context.stack.pop, context.stack.pop)
        
          inputs match {
            case (VarRef(indexVarName), VarRef(name)) => 
               val index = context.vars.resolveId(indexVarName).value.asInstanceOf[Int]
               val arrayValue = context.vars.resolveId(name).value.asInstanceOf[Array[Variable]](index)
               context.stack.push(arrayValue)
            case (index: Int, VarRef(name)) => 
              val arrayVar = context.vars.resolveId(name)
              context.stack.push(Address(arrayVar.address.address + index * TypeHelper.sizeof(arrayVar.typeName), arrayVar.typeName))
          }

          Seq()
      }
    case unary: IASTUnaryExpression =>
      import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression._
      
      def resolveVar(variable: Any, func: (Address) => Unit) = {
        variable match {
          case variable @ Variable(value) =>
            if (variable.refAddress != null) {
              println("POINTER DETECTED!!!!!!!!!!!!!")
              func(variable.refAddress)
            } else {
              func(variable.address)
            }
          case VarRef(name) =>
            val variable = context.vars.resolveId(name)
            
            if (variable.refAddress != null) {
              println("POINTER DETECTED!!!!!!!!!!!!!")
              func(variable.refAddress)
            } else {
              func(variable.address)
            }
          case x => x
        }
      }
      
      if (direction == Entering) {
        Seq(unary.getOperand)
      } else {
        unary.getOperator match {
          case `op_minus` =>  
            context.stack.pop match {
              case int: Int => context.stack.push(-int)
              case doub: Double => context.stack.push(-doub)
              case variable @ Variable(_) => resolveVar(variable, (address) => {
                
                context.stack.push(variable.typeName match {
                  case "int" => -Variable.data.getInt(address.address)
                  case "doub" => -Variable.data.getDouble(address.address)
                })
              })
            }
          case `op_postFixIncr` =>     
            resolveVar(context.stack.pop, (address) => {
              context.stack.push(Variable.data.getInt(address.address))
              Variable.data.putInt(address.address, Variable.data.getInt(address.address) + 1)
            })
          case `op_postFixDecr` =>
            resolveVar(context.stack.pop, (address) => {
              context.stack.push(Variable.data.getInt(address.address))
              Variable.data.putInt(address.address, Variable.data.getInt(address.address) - 1)
            })
          case `op_prefixIncr` =>  
            resolveVar(context.stack.pop, (address) => {
              Variable.data.putInt(address.address, Variable.data.getInt(address.address) + 1)
              context.stack.push(Variable.data.getInt(address.address))
            })
         case `op_prefixDecr` =>  
           resolveVar(context.stack.pop, (address) => {
              Variable.data.putInt(address.address, Variable.data.getInt(address.address) - 1)
              context.stack.push(Variable.data.getInt(address.address))
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
                context.stack.push(context.vars.resolveAddress(refAddress))
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
      // only evaluate after leaving
      if (direction == Exiting) {
        val name = call.getFunctionNameExpression match {
          case x: IASTIdExpression => x.getName.getRawSignature
          case _ => "Error"
        }
        
        val argList = call.getArguments.map { arg => (arg, context.stack.pop) }
        
        val resolved: Array[Any] = argList.map { case (arg, value) => 
          value match {
            case VarRef(name) => context.vars.resolveId(name).value
            case Variable(value) => value
            case Address(addy, typeName) => Variable.readVal(typeName, addy)
            case str: String => str
            case int: Int => int
            case doub: Double => doub
          }
        }

        if (name == "printf") {
          printf(context, resolved.map(_.asInstanceOf[Object]))
          Seq()
        } else {
          resolved.reverse.foreach { arg => context.stack.push(arg)}
          context.callFunction(call)
        }

      } else {
        call.getArguments.reverse
      }

    case bin: IASTBinaryExpression =>
      if (direction == Exiting) {
        val result = BinaryExpression.parse(bin, direction, context)
        if (result != null) {
          context.stack.push(result)
        }
        Seq()
      } else {
        Seq(bin.getOperand1, bin.getOperand2)
      }
  }
}