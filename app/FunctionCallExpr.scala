package app.astViewer

import org.eclipse.cdt.core.dom.ast.IASTFunctionCallExpression

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;
import Functions._
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType

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
              if (TypeHelper.isPointer(theVar.theType)) {
                theVar.value.asInstanceOf[Int]
              } else {
                state.readVal(theVar.address.value, TypeHelper.resolve(theVar.theType))
              } 
            case info @ AddressInfo(address, theType) => 
              if (TypeHelper.isPointer(theType)) {  
                address.value
              } else { 
                if (arg.isInstanceOf[IASTUnaryExpression] && arg.asInstanceOf[IASTUnaryExpression].getOperator == 5) {
                  address.value
                } else {
                  state.readVal(address.value, TypeHelper.resolve(theType))
                }
              }
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
          
          val formatString = formattedOutputParams.head.asInstanceOf[StringLiteral].str
          
          // here we resolve the addresses coming in
          val resolved = formattedOutputParams.map{x => 
            x match {
              case strLit: StringLiteral => strLit.str
              case addy: Int if formatString.contains("%s") => {
                    // its a string!
                    var current: Char = 0
                    var stringBuilder = new ListBuffer[Char]()
                    var i = 0
                    do {
                      current = stack.readVal(addy + i, new CBasicType(IBasicType.Kind.eChar, 0)).asInstanceOf[Char]
                      if (current != 0) {
                        stringBuilder += current
                        i += 1
                      }
                    } while (current != 0)
                      
                    new String(stringBuilder.map(_.toByte).toArray, "UTF-8")
                }
//                else {
//                    stack.readVal(addy.value, TypeHelper.resolve(theType))
//                }        
                
              case x => x
            }
          }
          
          Functions.printf(state, resolved.map(_.asInstanceOf[Object]))
          Seq()
        } else if (name == "strlen") {
          val straddy = formattedOutputParams.head.asInstanceOf[AddressInfo]
          var current: Char = 0
          var stringBuilder = new ListBuffer[Char]()
          var i = 0
          do {
            current = stack.readVal(straddy.address.value + i, TypeHelper.resolve(straddy.theType)).asInstanceOf[Char]
            if (current != 0) {
              stringBuilder += current
              i += 1
            }
          } while (current != 0)
          state.stack.push(i) 
          Seq()
        } else if (name == "rand") {
          state.stack.push(scala.util.Random.nextInt) 
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