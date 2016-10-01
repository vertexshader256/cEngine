package app.astViewer

import org.eclipse.cdt.core.dom.ast.IASTFunctionCallExpression

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;
import Functions._

object FunctionCallExpr {
  def parse(call: IASTFunctionCallExpression, direction: Direction, state: State, stack: State): Seq[IASTNode] = {
    if (direction == Exiting) {
        val name = call.getFunctionNameExpression match {
          case x: IASTIdExpression => x.getName.getRawSignature
          case _ => "Error"
        }
        
        val argList = call.getArguments.map { arg => (arg, state.stack.pop) }
        
        val formattedOutputParams: Array[Any] = argList.map { case (arg, value) => 
          value match {
            case VarRef(name) => 
              val theVar = state.vars.resolveId(name)
              if (theVar.isPointer) {
                Address(theVar.value.asInstanceOf[Int])
              } else {
                theVar.address
              } 
            case address @ Address(addy) => address
            case AddressInfo(address, _) => address
            case str: String => str
            case int: Int => int
            case float: Float => float
            case short: Short => short
            case long: Long => long
            case doub: Double => doub
            case char: Char => char.toInt.toByte
            case lit @ Literal(_) => lit.cast
          }
        }

        if (name == "printf") {
          
          // here we resolve the addresses coming in
          val resolved = formattedOutputParams.map{x => x match {
              case addy @ Address(address) =>
                val theType = stack.getType(addy)
                theType.toString match {
                  case "char" if state.getSize(addy) > 1 => 
                    var current: Char = 0
                    var stringBuilder = new ListBuffer[Char]()
                    var i = 0
                    do {
                      current = stack.readVal(address + i).asInstanceOf[Char]
                      if (current != 0) {
                        stringBuilder += current
                        i += 1
                      }
                    } while (current != 0)
                      
                    new String(stringBuilder.map(_.toByte).toArray, "UTF-8")
                  case "char" =>
                    stack.readVal(address).asInstanceOf[Char] & 0xFF
                  case _ => 
                    stack.readVal(address)
                }        
                
              case x => x
            }
          }
          
          Functions.printf(state, resolved.map(_.asInstanceOf[Object]))
          Seq()
        } else {
          // load up the stack with the parameters
          formattedOutputParams.reverse.foreach { arg => state.stack.push(arg)}
          state.callFunction(call)
        }

      } else {
        call.getArguments.reverse
      }
  }
}