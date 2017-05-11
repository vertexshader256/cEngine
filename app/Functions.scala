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
  
  def run(formattedOutputParams: Array[ValueInfo], state: State): Option[AnyVal]
  def getNext: IASTNode = null
}

object Functions {
  
  var varArgStartingAddr = 0
  
  val scalaFunctions = List[Function](
      new Function("rand", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          Some(Math.abs(scala.util.Random.nextInt))
        }
      },
      new Function("isalpha", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          val theChar = formattedOutputParams.head.value.asInstanceOf[Character].toChar
          Some(if (theChar.isLetter) 1 else 0)
        }
      },
      new Function("tolower", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          val theChar = formattedOutputParams.head.value.asInstanceOf[Character].toChar
          Some(theChar.toLower.toByte)
        }
      },
      new Function("toupper", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          val theChar = formattedOutputParams.head.value.asInstanceOf[Character].toChar
          Some(theChar.toUpper.toByte)
        }
      },
      new Function("isupper", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          val theChar = formattedOutputParams.head.value match {
            case int: Int => int.toChar
            case char: Character => char.toChar
          }
          Some(if (theChar.isUpper) 1 else 0)
        }
      },
      new Function("calloc", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          val numBlocks = formattedOutputParams(0).value.asInstanceOf[Int]
          val blockSize = formattedOutputParams(1).value.asInstanceOf[Int]
          val addr = state.allocateHeapSpace(numBlocks * blockSize)
          Some(addr)
        }
      },
      new Function("malloc", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          val returnVal = formattedOutputParams.head.value match {
            case long: Long => state.allocateHeapSpace(long.toInt)
            case int: Int => state.allocateHeapSpace(int)
          }
          Some(returnVal)
        }
      },
      new Function("realloc", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          Some(state.allocateHeapSpace(formattedOutputParams.head.value.asInstanceOf[Int]))
        }
      },
      new Function("memmove", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          val dst = formattedOutputParams(0).value.asInstanceOf[Int]
          val src = formattedOutputParams(1).value.asInstanceOf[Int]
          val numBytes = formattedOutputParams(2).value.asInstanceOf[Int]
          
          state.copy(dst, src, numBytes)
          None
        }
      },
      new Function("memcpy", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          val numBytes = formattedOutputParams(0).value match {
            case int: Int => int
            case long: Long => long.toInt
          }
          val src = formattedOutputParams(1).value.asInstanceOf[Int]
          val dst = formattedOutputParams(2).value.asInstanceOf[Int]

          state.copy(dst, src, numBytes)
          None
        }
      },
      new Function("_assert", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State): Option[AnyVal] = {
          val addy = formattedOutputParams(0).value.asInstanceOf[Int]
          println(Utils.readString(addy)(state) + " FAILED")
          None
        }
      },
      new Function("modf", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          val fraction = formattedOutputParams(0).value.asInstanceOf[Double]
          val intPart = formattedOutputParams(1).value.asInstanceOf[Int]
          
          state.setValue(fraction.toInt, intPart)
          
          Some(fraction % 1.0)
        }
      },
      new Function("putchar", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          val char = formattedOutputParams(0).value.asInstanceOf[Character].toChar

          if (char == 10) {
            if (standardOutBuffer.isEmpty) {
              state.stdout += ""
            } else {
              state.stdout += standardOutBuffer.mkString
              standardOutBuffer.clear
            }
          } else {
            standardOutBuffer += char
          }

          None
        }
      },
      new Function("printf", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {

          val str = Utils.readString(formattedOutputParams.last.value.asInstanceOf[Int])(state)
          val formatString = str.replaceAll("^\"|\"$", "").replaceAll("%ld", "%d").replaceAll("%l", "%d")

          val buffer = new StringBuffer()
          val formatter = new Formatter(buffer, Locale.US)

          var percentFound = false
          var paramCount = 0

          val resolved = new ListBuffer[Object]()

          val varArgs = formattedOutputParams.reverse.tail.toList

          str.toCharArray.foreach{ c =>
            if (!percentFound && c == '%') {
              percentFound = true
            } else if (percentFound && c == 's') {
              percentFound = false
              val theVal = varArgs(paramCount).value
              val stringAddr = theVal.asInstanceOf[Int]
              val str = Utils.readString(stringAddr)(state)
              resolved += str.split(System.lineSeparator()).mkString.asInstanceOf[Object]
              paramCount += 1
            } else if (percentFound && c == 'd') {
              val x = TypeHelper.resolve(varArgs(paramCount))(state).value
              resolved += (if (x.isInstanceOf[Boolean]) {
                if (x.asInstanceOf[Boolean]) 1 else 0
              } else {
                x
              }).asInstanceOf[Object]

              percentFound = false
              paramCount += 1
            } else if (percentFound && c == 'c') {
              resolved += TypeHelper.resolve(varArgs(paramCount))(state).value.asInstanceOf[Object]
              percentFound = false
              paramCount += 1
            } else if (percentFound && c == 'f') {
              resolved += TypeHelper.resolve(varArgs(paramCount))(state).value.asInstanceOf[Object]
              percentFound = false
              paramCount += 1
            }
          }

          formatter.format(formatString, resolved: _*)

          buffer.toString.getBytes.foreach{char =>
            state.callFunctionFromScala("putchar", Array(new ValueInfo(char, new CBasicType(IBasicType.Kind.eChar, 0))))
          }

          None
        }
      },
      new Function("strlen", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          val straddy = formattedOutputParams.head.value match {
            //case AddressInfo(addr, _) => addr.value
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
    new Function("strcmp", false) {
      def run(formattedOutputParams: Array[ValueInfo], state: State) = {
        val straddy = formattedOutputParams(0).value.asInstanceOf[Int]
        val straddy2 = formattedOutputParams(1).value.asInstanceOf[Int]
        val same = Utils.readString(straddy)(state) == Utils.readString(straddy2)(state)
        Some((if (same) 0 else 1))
      }
    },
    new Function("memcmp", false) {
      def run(formattedOutputParams: Array[ValueInfo], state: State) = {
        val numBytes = formattedOutputParams(0).value.asInstanceOf[Int]
        val memaddy = formattedOutputParams(1).value.asInstanceOf[Int]
        val memaddy2 = formattedOutputParams(2).value.asInstanceOf[Int]

        var same = true

        for (i <- (0 until numBytes)) {
          same &= state.readVal(memaddy + i, new CBasicType(IBasicType.Kind.eChar, 0)) == state.readVal(memaddy2 + i, new CBasicType(IBasicType.Kind.eChar, 0))
        }

        Some((if (same) 0 else 1))
      }
    },
      new Function("free", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State): Option[AnyVal] = {
          None
        }
      },
      new Function("va_arg", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          val argTypeStr = formattedOutputParams(0).value.asInstanceOf[Int]
          
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
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          val lastNamedArgAddr = formattedOutputParams(0).value.asInstanceOf[Int]
          val listAddr = formattedOutputParams(1).value.asInstanceOf[Int]
          varArgStartingAddr = lastNamedArgAddr + 4
          None
        }
      },
      new Function("va_end", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
           None
        }
      },
      //fcvtbuf(double arg, int ndigits, int *decpt, int *sign, char *buf)
      new Function("fcvtbuf", false) {
        def run(formattedOutputParams: Array[ValueInfo], state: State) = {
          val arg = formattedOutputParams(4).asInstanceOf[Double]
          val precision = formattedOutputParams(3).asInstanceOf[Int]
          val decpt = formattedOutputParams(2).asInstanceOf[Int]
          
          state.setValue(1, decpt)
          
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

  var standardOutBuffer = new ListBuffer[Char]
}