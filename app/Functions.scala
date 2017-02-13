package app.astViewer

import org.eclipse.cdt.core.dom.ast._
import java.util.Formatter
import java.util.Locale;
import scala.collection.mutable.HashMap
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType

abstract case class Function(name: String, index: Int) {
  def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode
}

object Functions {
  
  var standardOutBuffer = ""
  var lastChar: Byte = 0
  
  val functionMap = List[Function](
      new Function("rand", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
          state.stack.push(Math.abs(scala.util.Random.nextInt))
          null
        }
      },
      new Function("isalpha", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
          val theChar = formattedOutputParams.head.asInstanceOf[Character].toChar
          state.stack.push(if (theChar.isLetter) 1 else 0) 
          null
        }
      },
      new Function("tolower", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
          val theChar = formattedOutputParams.head.asInstanceOf[Character].toChar
          state.stack.push(theChar.toLower.toByte)
          null
        }
      },
      new Function("isupper", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
          val theChar = formattedOutputParams.head.asInstanceOf[Character].toChar
          state.stack.push(if (theChar.isUpper) 1 else 0)
          null
        }
      },
      new Function("calloc", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
          val numBlocks = formattedOutputParams(0).asInstanceOf[Int]
          val blockSize = formattedOutputParams(1).asInstanceOf[Int]
          val addr = state.allocateSpace(numBlocks * blockSize)
          state.stack.push(addr)
          null
        }
      },
      new Function("malloc", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
          formattedOutputParams.head match {
            case long: Long => state.stack.push(state.allocateSpace(long.toInt))
            case int: Int => state.stack.push(state.allocateSpace(int))
          }
          null
        }
      },
      new Function("realloc", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
          state.stack.push(state.allocateSpace(formattedOutputParams.head.asInstanceOf[Int]))
          null
        }
      },
      new Function("memmove", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
          val dst = formattedOutputParams(0).asInstanceOf[Address]
          val src = formattedOutputParams(1).asInstanceOf[Address]
          val numBytes = formattedOutputParams(2).asInstanceOf[Int]
          
          state.move(dst, src, numBytes)
          null
        }
      },
      new Function("_assert", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
          val addy = formattedOutputParams(0).asInstanceOf[Address]
          println(state.readString(addy) + " FAILED")
          null
        }
      },
      new Function("putchar", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
          val theChar = formattedOutputParams(0).asInstanceOf[Character]
          if (theChar == 'n' && lastChar == '\\') {
            state.stdout += standardOutBuffer
            standardOutBuffer = ""
          } else if (theChar != '\\') {
            standardOutBuffer += theChar.toChar
          }
          lastChar = theChar
          null
        }
      },
      new Function("printf", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
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
          null
        }
      },
      new Function("strlen", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
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
          null
        }
      },
      new Function("free", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
          null
        }
      },
      new Function("__builtin_va_start", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
          null
        }
      },
      new Function("__builtin_va_end", 0) {
        def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = {
          null
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