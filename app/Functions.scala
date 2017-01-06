package app.astViewer

import org.eclipse.cdt.core.dom.ast._
import java.util.Formatter
import java.util.Locale;
import scala.collection.mutable.HashMap
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType

abstract case class Function(name: String) {
  def run(formattedOutputParams: Array[AnyVal], state: State)
}

object Functions {
  
  var standardOutBuffer = ""
  var lastChar: Byte = 0
  
  val functionMap = List[Function](
      new Function("rand") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
          state.stack.push(Math.abs(scala.util.Random.nextInt))
        }
      },
      new Function("isalpha") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
          val theChar = formattedOutputParams.head.asInstanceOf[Character].toChar
          state.stack.push(if (theChar.isLetter) 1 else 0) 
        }
      },
      new Function("tolower") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
          val theChar = formattedOutputParams.head.asInstanceOf[Character].toChar
          state.stack.push(theChar.toLower.toByte) 
        }
      },
      new Function("isupper") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
          val theChar = formattedOutputParams.head.asInstanceOf[Character].toChar
          state.stack.push(if (theChar.isUpper) 1 else 0) 
        }
      },
      new Function("calloc") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
          val numBlocks = formattedOutputParams(0).asInstanceOf[Int]
          val blockSize = formattedOutputParams(1).asInstanceOf[Int]
          val addr = state.allocateSpace(numBlocks * blockSize)
          state.stack.push(addr)
        }
      },
      new Function("malloc") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
          formattedOutputParams.head match {
            case long: Long => state.stack.push(state.allocateSpace(long.toInt))
            case int: Int => state.stack.push(state.allocateSpace(int))
          }
        }
      },
      new Function("realloc") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
          state.stack.push(state.allocateSpace(formattedOutputParams.head.asInstanceOf[Int]))
        }
      },
      new Function("memmove") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
          val dst = formattedOutputParams(0).asInstanceOf[Address]
          val src = formattedOutputParams(1).asInstanceOf[Address]
          val numBytes = formattedOutputParams(2).asInstanceOf[Int]
          
          state.move(dst, src, numBytes)
        }
      },
      new Function("_assert") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
          val addy = formattedOutputParams(0).asInstanceOf[Address]
          println(state.readString(addy) + " FAILED")
        }
      },
      new Function("putchar") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
          val theChar = formattedOutputParams(0).asInstanceOf[Character]
          if (theChar == 'n' && lastChar == '\\') {
            state.stdout += standardOutBuffer
            standardOutBuffer = ""
          } else if (theChar != '\\') {
            standardOutBuffer += theChar.toChar
          }
          lastChar = theChar
        }
      },
      new Function("printf") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
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
        }
      },
      new Function("strlen") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
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
        }
      },
      new Function("free") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
        }
      },
      new Function("__builtin_va_start") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
        }
      },
      new Function("__builtin_va_end") {
        def run(formattedOutputParams: Array[AnyVal], state: State) {
        }
      }  
  )
  
  def printf(context: State, args: Seq[Object]) = {
    val formatString = args.head.asInstanceOf[String].replaceAll("^\"|\"$", "").replaceAll("%ld", "%d").replaceAll("%l", "%d")
 
    val buffer = new StringBuffer();
    val formatter = new Formatter(buffer, Locale.US);
    
    val resolvedStrings = args.tail.map{ _ match {
      case str: String => str.split("""\"""").mkString
      case x => x 
    }}.toArray
    
    formatter.format(formatString, resolvedStrings: _*)
    
    context.stdout ++= buffer.toString.split("""\\n""")
  }
}