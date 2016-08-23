package app.astViewer

import org.eclipse.cdt.core.dom.ast.IASTFunctionCallExpression

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;
import Functions._

object FunctionCallExpr {
  def parse(call: IASTFunctionCallExpression, direction: Direction, context: IASTContext): Seq[IASTNode] = {
    if (direction == Exiting) {
        val name = call.getFunctionNameExpression match {
          case x: IASTIdExpression => x.getName.getRawSignature
          case _ => "Error"
        }
        
        val argList = call.getArguments.map { arg => (arg, context.stack.pop) }
        
        val formattedOutputParams: Array[Any] = argList.map { case (arg, value) => 
          value match {
            case VarRef(name) => context.vars.resolveId(name).address
            case address @ Address(addy, typeName) => address
            case str: String => str
            case int: Int => int
            case doub: Double => doub
          }
        }

        if (name == "printf") {
          
          // here we resolve the addresses coming in
          val resolved = formattedOutputParams.map{x => x match {
              case addy @ Address(address, typeName) =>
                
                val vari = context.vars.resolveAddress(addy)
                
                typeName match {
                  case "char" => 
                    if (vari.numElements > 1) {
                      var current: Char = 0
                      var stringBuilder = new ListBuffer[Char]()
                      var i = 0
                      do {
                        current = Variable.readVal(typeName, address + i).asInstanceOf[Char]
                        if (current != 0) {
                          stringBuilder += current
                          i += 1
                        }
                      } while (current != 0)
                      new String(stringBuilder.map(_.toByte).toArray, "UTF-8")
                    }
                  case _ => Variable.readVal(typeName, address)
                }        
                
              case x => x
            }
          }
          
          Functions.printf(context, resolved.map(_.asInstanceOf[Object]))
          Seq()
        } else {
          // load up the stack with the parameters
          formattedOutputParams.reverse.foreach { arg => context.stack.push(arg)}
          context.callFunction(call)
        }

      } else {
        call.getArguments.reverse
      }
  }
}