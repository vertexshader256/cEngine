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
  
  var standardOutBuffer = ""
  var lastChar: Byte = 0
  val varArgs = new ListBuffer[Any]()
  
  def parse(call: IASTFunctionCallExpression, direction: Direction)(implicit state: State): Seq[IASTNode] = {
    if (direction == Exiting) {
        val name = state.stack.pop match {
          case VarRef(str) => str
        }
        
        val argList = call.getArguments.map { arg => (arg, state.stack.pop) }
        
        val formattedOutputParams: Array[AnyVal] = argList.map { case (arg, value) => 
          
          value match {
            case Variable(theInfo: Variable[_]) => 
              val info = theInfo.info
              if (TypeHelper.isPointer(info.theType) && TypeHelper.getPointedType(info.theType).isInstanceOf[IBasicType] &&
                  TypeHelper.getPointedType(info.theType).asInstanceOf[IBasicType].getKind == IBasicType.Kind.eChar) {
                Address(state.readVal(info.address, info.theType).value.asInstanceOf[Int])
              } else {
                state.readVal(info.address, info.theType).value
              }
            case Address(address) =>
              address
            case ValueInfo(theVal, _) =>             
              theVal
            case AddressInfo(address, theType) => 
              state.readVal(address, TypeHelper.resolve(theType)).value
            case bool: Boolean => if (bool) 1 else 0
            case int: Int => int
            case float: Float => float
            case short: Short => short
            case long: Long => long
            case doub: Double => doub
            case char: Character => char
            case lit @ Literal(_) => lit.cast.value
            case StringLiteral(str) => 
              val strAddr = state.createStringVariable(str)
              strAddr
          }
        }

        if (name == "printf") {

          // here we resolve the addresses coming in
          val resolved = formattedOutputParams.map{x => 
            x match {
              case strLit: StringLiteral => strLit.str
              case addy @ Address(addr) => {
                  // its a string!
                state.readString(addy)
              }
              case x => x
            }
          }
          
          Functions.printf(state, resolved.map(_.asInstanceOf[Object]))
          Seq()
        } else if (name == "strlen") {
          val straddy = formattedOutputParams.head match {
            //case AddressInfo(addr, _) => addr.value
            case Address(addr) => addr
            case int: Int => int
          }
          var current: Character = 0
          var i = 0
          do {
            current = state.readVal(Address(straddy + i), new CBasicType(IBasicType.Kind.eChar, 0)).value.asInstanceOf[Character]
            if (current != 0) {
              i += 1
            }
          } while (current != 0)
          state.stack.push(i) 
          Seq()
        } else if (name == "rand") {
          state.stack.push(Math.abs(scala.util.Random.nextInt)) 
          Seq()
        } else if (name == "isalpha") {
          val theChar = formattedOutputParams.head.asInstanceOf[Character].toChar
          state.stack.push(if (theChar.isLetter) 1 else 0) 
          Seq()
        } else if (name == "tolower") {
          val theChar = formattedOutputParams.head.asInstanceOf[Character].toChar
          state.stack.push(theChar.toLower.toByte) 
          Seq()
        } else if (name == "isupper") {
          val theChar = formattedOutputParams.head.asInstanceOf[Character].toChar
          state.stack.push(if (theChar.isUpper) 1 else 0) 
          Seq()
        } else if (name == "calloc") {
          val numBlocks = formattedOutputParams(0).asInstanceOf[Int]
          val blockSize = formattedOutputParams(1).asInstanceOf[Int]
          val addr = state.allocateSpace(numBlocks * blockSize)
          state.stack.push(addr)
         // state.clearMemory(addr, numBlocks * blockSize)
          Seq()
        } else if (name == "malloc") {
          state.stack.push(state.allocateSpace(formattedOutputParams.head.asInstanceOf[Int]))
          Seq()
        } else if (name == "free") {
          // TODO
          Seq()
        } else if (name == "__builtin_va_start") {
          //varArgs += state.stack.pop
          //varArgs += state.stack.pop
          Seq()
        } else if (name == "__builtin_va_end") {
          Seq()
        } else if (name == "putchar") {
          val theChar = formattedOutputParams(0).asInstanceOf[Character]
          
          
          if (theChar == 'n' && lastChar == '\\') {
            state.stdout += standardOutBuffer
            standardOutBuffer = ""
          } else if (theChar != '\\') {
            standardOutBuffer += theChar.toChar
          }
          lastChar = theChar
          
          Seq()
        } 
        else {
          state.callFunction(call, formattedOutputParams)
        }

      } else {
        call.getArguments.reverse ++ Seq(call.getFunctionNameExpression)
      }
  }
}