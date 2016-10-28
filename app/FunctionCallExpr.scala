package app.astViewer

import org.eclipse.cdt.core.dom.ast.IASTFunctionCallExpression

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;
import Functions._
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType
import org.eclipse.cdt.internal.core.dom.parser.c.CStructure

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
            case Address(address) =>
              address
            case info @ AddressInfo(address, theType) =>
              state.readVal(address.value, TypeHelper.resolve(theType))
            case bool: Boolean => if (bool) 1 else 0
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
          state.stack.push(Math.abs(scala.util.Random.nextInt)) 
          Seq()
        } else if (name == "calloc") {
          state.stack.push(state.allocateSpace(formattedOutputParams.head.asInstanceOf[Int]))
          Seq()
        } else if (name == "malloc") {
          state.stack.push(state.allocateSpace(formattedOutputParams.head.asInstanceOf[Int]))
          Seq()
        } else if (name == "free") {
          // TODO
          Seq()
        } else {
          state.callFunction(call, formattedOutputParams)
        }

      } else {
        call.getArguments.reverse
      }
  }
}