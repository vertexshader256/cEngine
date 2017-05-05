package app.astViewer

import org.eclipse.cdt.core.dom.ast._
import java.util.Formatter
import java.util.Locale;
import scala.collection.mutable.HashMap
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType
import scala.collection.mutable.ListBuffer
import org.eclipse.cdt.internal.core.dom.parser.c.CPointerType

// 'isNative' implies the function is in C, not Scala
abstract case class Function(name: String, isNative: Boolean) {
  
  var index = -1
  
  def run(formattedOutputParams: Array[AnyVal], state: State): Option[AnyVal]
  def getNext: IASTNode = null
}

object Functions {
  
  var varArgStartingAddr = 0
  
  val scalaFunctions = List[Function](
      new Function("rand", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          Some(Math.abs(scala.util.Random.nextInt))
        }
      },
      new Function("isalpha", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          val theChar = formattedOutputParams.head.asInstanceOf[Character].toChar
          Some(if (theChar.isLetter) 1 else 0)
        }
      },
      new Function("tolower", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          val theChar = formattedOutputParams.head.asInstanceOf[Character].toChar
          Some(theChar.toLower.toByte)
        }
      },
      new Function("toupper", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          val theChar = formattedOutputParams.head.asInstanceOf[Character].toChar
          Some(theChar.toUpper.toByte)
        }
      },
      new Function("isupper", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          val theChar = formattedOutputParams.head.asInstanceOf[Character].toChar
          Some(if (theChar.isUpper) 1 else 0)
        }
      },
      new Function("calloc", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          val numBlocks = formattedOutputParams(0).asInstanceOf[Int]
          val blockSize = formattedOutputParams(1).asInstanceOf[Int]
          val addr = state.allocateHeapSpace(numBlocks * blockSize)
          Some(addr)
        }
      },
      new Function("malloc", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          val returnVal = formattedOutputParams.head match {
            case long: Long => state.allocateHeapSpace(long.toInt)
            case int: Int => state.allocateHeapSpace(int)
          }
          Some(returnVal)
        }
      },
      new Function("realloc", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          Some(state.allocateHeapSpace(formattedOutputParams.head.asInstanceOf[Int]))
        }
      },
      new Function("memmove", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          val dst = formattedOutputParams(0).asInstanceOf[Address]
          val src = formattedOutputParams(1).asInstanceOf[Address]
          val numBytes = formattedOutputParams(2).asInstanceOf[Int]
          
          state.copy(dst.value, src.value, numBytes)
          None
        }
      },
      new Function("memcpy", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          val dst = formattedOutputParams(0).asInstanceOf[Int]
          val src = formattedOutputParams(1).asInstanceOf[Int]
          val numBytes = formattedOutputParams(2).asInstanceOf[Int]

          state.copy(dst, src, numBytes)
          None
        }
      },
      new Function("_assert", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State): Option[AnyVal] = {
          val addy = formattedOutputParams(0).asInstanceOf[Address]
          println(Utils.readString(addy)(state) + " FAILED")
          None
        }
      },
      new Function("modf", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          val fraction = formattedOutputParams(0).asInstanceOf[Double]
          val intPart = formattedOutputParams(1).asInstanceOf[Int]
          
          state.setValue(fraction.toInt, Address(intPart))
          
          Some(fraction % 1.0)
        }
      },
      new Function("putchar", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          val theChar = formattedOutputParams(0).asInstanceOf[Character]
          if (theChar == 10) {
            state.stdout += Functions.standardOutBuffer
            Functions.standardOutBuffer = ""
          } else {
            Functions.standardOutBuffer += theChar.toChar
          }
          None
        }
      },
      new Function("printf", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {

          val str = Utils.readString(formattedOutputParams.last.asInstanceOf[Address])(state)
          val formatString = str.replaceAll("^\"|\"$", "").replaceAll("%ld", "%d").replaceAll("%l", "%d").replaceAll(10.asInstanceOf[Char].toString, System.lineSeparator())

          val buffer = new StringBuffer()
          val formatter = new Formatter(buffer, Locale.US)

          val resolved = formattedOutputParams.reverse.tail.map{x => x match {
            case addy @ Address(addr) => {
              // its a string!
              val str = Utils.readString(addy)(state)
              val resolved = str.replaceAll(10.asInstanceOf[Char].toString, System.lineSeparator())
              resolved.split(System.lineSeparator()).mkString
            }
            case x => TypeHelper.resolve(x)(state).value
          }}.map{_.asInstanceOf[Object]}

          formatter.format(formatString, resolved: _*)

          state.stdout ++= buffer.toString.split(System.lineSeparator())
          None
        }
      },
      new Function("strlen", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          val straddy = formattedOutputParams.head match {
            //case AddressInfo(addr, _) => addr.value
            case Address(addr) => addr
            case int: Int => int
          }
          var current: Character = 0
          var i = 0
          do {
            current = state.readVal(straddy + i, new CBasicType(IBasicType.Kind.eChar, 0)).value.asInstanceOf[Character]
            if (current != 0) {
              i += 1
            }
          } while (current != 0)
          Some(i)
        }
      },
      new Function("free", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State): Option[AnyVal] = {
          None
        }
      },
      new Function("va_arg", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          val argTypeStr = formattedOutputParams(0).asInstanceOf[Address]
          
          val str = Utils.readString(argTypeStr)(state)
          
          val (offset, theType) = (str match {
            case "int" => (4, TypeHelper.pointerType)
            case "double" => (8, new CBasicType(IBasicType.Kind.eDouble, 0))
            case "char" => (1, TypeHelper.pointerType)
            case _ => (4, TypeHelper.pointerType)
          })
          
          val result = state.readVal(varArgStartingAddr, theType).value
          println("RESULT: " + result)

          
          varArgStartingAddr += offset
          Some(result)
        }
      },
      new Function("va_start", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          val lastNamedArgAddr = formattedOutputParams(0).asInstanceOf[Int]
          val listAddr = formattedOutputParams(1).asInstanceOf[Int]
          varArgStartingAddr = lastNamedArgAddr + 4
          None
        }
      },
      new Function("va_end", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
           None
        }
      },
      //fcvtbuf(double arg, int ndigits, int *decpt, int *sign, char *buf)
      new Function("fcvtbuf", false) {
        def run(formattedOutputParams: Array[AnyVal], state: State) = {
          val arg = formattedOutputParams(4).asInstanceOf[Double]
          val precision = formattedOutputParams(3).asInstanceOf[Int]
          val decpt = formattedOutputParams(2).asInstanceOf[Int]
          
          state.setValue(1, Address(decpt))
          
          val buffer = new StringBuffer();
          val formatter = new Formatter(buffer, Locale.US);
          
          val formatString = "%." + precision + "f"
          
          val args = Array[Object](arg.asInstanceOf[Object])
          
          formatter.format(formatString, args: _*)
          
          val result = buffer.toString.split(System.lineSeparator()).mkString

          val newStr = Utils.allocateString(StringLiteral("\"" + result.replaceAll("\\.", "") + "\""), false)(state)
          Some(newStr.value)
        }
      }
  )

  var standardOutBuffer = ""
}