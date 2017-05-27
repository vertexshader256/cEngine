package c.engine

import org.eclipse.cdt.core.dom.ast._
import java.util.Formatter
import java.util.Locale

import scala.collection.mutable.HashMap
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.collection.mutable.ListBuffer

// 'isNative' implies the function is in C, not Scala
abstract case class Function(name: String, isNative: Boolean) {
  var index = -1

  val staticVars: List[Variable] = List()

  def run(formattedOutputParams: Array[RValue], state: State): Option[AnyVal]
  def node: IASTNode = null
}

object Functions {
  
  var varArgStartingAddr = 0
  
  val scalaFunctions = new ListBuffer[Function]()
  
  scalaFunctions += new Function("rand", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          Some(Math.abs(scala.util.Random.nextInt))
        }
      }
  
  scalaFunctions += new Function("isalpha", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          val theChar = formattedOutputParams.head.value.asInstanceOf[char].toChar
          Some(if (theChar.isLetter) 1 else 0)
        }
      }
  
  scalaFunctions += new Function("tolower", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          val theChar = formattedOutputParams.head.value.asInstanceOf[char].toChar
          Some(theChar.toLower.toByte)
        }
      }
  
  scalaFunctions += new Function("toupper", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          val theChar = formattedOutputParams.head.value.asInstanceOf[char].toChar
          Some(theChar.toUpper.toByte)
        }
      }
  
  scalaFunctions += new Function("isupper", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          val theChar = formattedOutputParams.head.value match {
            case int: int => int.toChar
            case char: char => char.toChar
          }
          Some(if (theChar.isUpper) 1 else 0)
        }
      }
  
  scalaFunctions += new Function("calloc", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          val numBlocks = formattedOutputParams(0).value.asInstanceOf[Int]
          val blockSize = formattedOutputParams(1).value.asInstanceOf[Int]
          val addr = state.allocateHeapSpace(numBlocks * blockSize)
          Some(addr)
        }
      }
  
  scalaFunctions += new Function("malloc", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          val returnVal = formattedOutputParams.head.value match {
            case long: Long => state.allocateHeapSpace(long.toInt)
            case int: Int => state.allocateHeapSpace(int)
          }
          Some(returnVal)
        }
      }
  
 scalaFunctions += new Function("realloc", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          Some(state.allocateHeapSpace(formattedOutputParams.head.value.asInstanceOf[Long].toInt))
        }
      }
 
  scalaFunctions += new Function("memmove", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          val dst = formattedOutputParams(0).value.asInstanceOf[Int]
          val src = formattedOutputParams(1).value.asInstanceOf[Int]
          val numBytes = formattedOutputParams(2).value.asInstanceOf[Int]
          
          state.copy(dst, src, numBytes)
          None
        }
      }
  
  scalaFunctions += new Function("memcpy", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          val numBytes = formattedOutputParams(0).value match {
            case int: Int => int
            case long: Long => long.toInt
          }
          val src = formattedOutputParams(1).value.asInstanceOf[Int]
          val dst = formattedOutputParams(2).value.asInstanceOf[Int]

          state.copy(dst, src, numBytes)
          None
        }
      }
  
   scalaFunctions += new Function("_assert", false) {
        def run(formattedOutputParams: Array[RValue], state: State): Option[AnyVal] = {
          val addy = formattedOutputParams(0).value.asInstanceOf[Int]
          println(Utils.readString(addy)(state) + " FAILED")
          None
        }
      }
   
   scalaFunctions += new Function("modf", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          val fraction = formattedOutputParams(0).value.asInstanceOf[Double]
          val intPart = formattedOutputParams(1).value.asInstanceOf[Int]
          
          state.setValue(fraction.toInt, intPart)
          
          Some(fraction % 1.0)
        }
      }
   
   scalaFunctions += new Function("putchar", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          val char = formattedOutputParams(0).value.asInstanceOf[char].toChar

          printf(char.toString)

          if (char == 10) {
            if (state.standardOutBuffer.isEmpty) {
              state.stdout += ""
            } else {
              state.stdout += state.standardOutBuffer.mkString
              state.standardOutBuffer.clear
            }
          } else {
            state.standardOutBuffer += char
          }

          None
        }
      }
   
   scalaFunctions += new Function("printf", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {

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

              if (stringAddr != 0) {
                val str = Utils.readString(stringAddr)(state)
                resolved += str.split(System.lineSeparator()).mkString.asInstanceOf[Object]
              } else {
                resolved += "(null)".asInstanceOf[Object]
              }
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
            state.callFunctionFromScala("putchar", Array(new RValue(char, new CBasicType(IBasicType.Kind.eChar, 0))))
          }

          None
        }
      }
   
   scalaFunctions += new Function("strlen", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          val straddy = formattedOutputParams.head.value match {
            //case AddressInfo(addr, _) => addr.value
            case int: Int => int
          }
          var current: char = 0
          var i = 0
          do {
            current = state.readVal(straddy + i, new CBasicType(IBasicType.Kind.eChar, 0)).value.asInstanceOf[char]
            if (current != 0) {
              i += 1
            }
          } while (current != 0)
          Some(i)
        }
      }
   
  scalaFunctions += new Function("strchr", false) {
      def run(formattedOutputParams: Array[RValue], state: State) = {
        val char = formattedOutputParams(0).value.asInstanceOf[Int]
        val straddy = formattedOutputParams(1).value.asInstanceOf[Int]

        val str = Utils.readString(straddy)(state)

        val offset = str.indexOf(char.toChar)

        if (offset != -1) {
          Some(straddy + offset)
        } else {
          Some(0)
        }
      }
    }
  
  scalaFunctions += new Function("offsetof", false) {
      def run(formattedOutputParams: Array[RValue], state: State) = {
        val straddy = formattedOutputParams(0).value.asInstanceOf[Int]
        val straddy2 = formattedOutputParams(1).value.asInstanceOf[Int]

        val memberName = Utils.readString(straddy)(state)
        val stuctName = Utils.readString(straddy2)(state)

        val structs = state.tUnit.getDeclarations.collect{case simp: CASTSimpleDeclaration => simp.getDeclSpecifier}
                                   .collect{case comp: CASTCompositeTypeSpecifier => comp}
                                   .map{x => x.getName.resolveBinding().asInstanceOf[CStructure]}

        val struct = structs.find{x => ("struct " + x.getName) == stuctName}.get

        Some(TypeHelper.offsetof(struct, memberName))
      }
    }
  
 scalaFunctions += new Function("strcmp", false) {
      def run(formattedOutputParams: Array[RValue], state: State) = {
        val straddy = formattedOutputParams(0).value.asInstanceOf[Int]
        val straddy2 = formattedOutputParams(1).value.asInstanceOf[Int]

        val str1 = Utils.readString(straddy)(state)
        val str2 = Utils.readString(straddy2)(state)

        println(str1 + " : " + str2)

        val same = str1 == str2
        Some((if (same) 0 else 1))
      }
    }
 
  scalaFunctions += new Function("memcmp", false) {
      def run(formattedOutputParams: Array[RValue], state: State) = {
        val numBytes = formattedOutputParams(0).value.asInstanceOf[Long].toInt
        val memaddy = formattedOutputParams(1).value.asInstanceOf[int]
        val memaddy2 = formattedOutputParams(2).value.asInstanceOf[int]

        var same = true

        for (i <- (0 until numBytes)) {
          same &= state.readVal(memaddy + i, new CBasicType(IBasicType.Kind.eChar, 0)).value == state.readVal(memaddy2 + i, new CBasicType(IBasicType.Kind.eChar, 0)).value
        }

        Some((if (same) 0 else 1))
      }
    }
  
  scalaFunctions += new Function("free", false) {
        def run(formattedOutputParams: Array[RValue], state: State): Option[AnyVal] = {
          None
        }
      }
  
  scalaFunctions += new Function("va_arg", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          val argTypeStr = formattedOutputParams(0).value.asInstanceOf[Int]
          
          val str = Utils.readString(argTypeStr)(state)
          
          val (offset, theType) = (str match {
            case "unsigned int" => (4, new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_UNSIGNED))
            case "int" => (4, TypeHelper.pointerType)
            case "double" => (8, new CBasicType(IBasicType.Kind.eDouble, 0))
            case "char" => (1, new CBasicType(IBasicType.Kind.eChar, 0))
            case "char *" => (4, new CPointerType(new CBasicType(IBasicType.Kind.eChar, 0), 0))
          })
          
          val result = state.readVal(varArgStartingAddr, theType).value

          varArgStartingAddr += offset
          Some(result)
        }
      }
  
   scalaFunctions += new Function("va_start", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          val lastNamedArgAddr = formattedOutputParams(0).value.asInstanceOf[Int]
          val listAddr = formattedOutputParams(1).value.asInstanceOf[Int]
          varArgStartingAddr = lastNamedArgAddr + 4
          None
        }
      }
   
   scalaFunctions += new Function("va_end", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
           None
        }
      }
      //fcvtbuf(double arg, int ndigits, int *decpt, int *sign, char *buf)
  scalaFunctions += new Function("fcvtbuf", false) {
        def run(formattedOutputParams: Array[RValue], state: State) = {
          val buf = formattedOutputParams(0).value.asInstanceOf[Int]
          val sign = formattedOutputParams(1).value.asInstanceOf[Int]
          val decpt = formattedOutputParams(2).value.asInstanceOf[Int]
          val ndigits = formattedOutputParams(3).value.asInstanceOf[Int]
          val arg = formattedOutputParams(4).value.asInstanceOf[Double]
          
          state.setValue(1, decpt)
          
          val buffer = new StringBuffer();
          val formatter = new Formatter(buffer, Locale.US);
          
          val formatString = "%." + ndigits + "f"
          
          val args = Array[Object](arg.asInstanceOf[Object])
          
          formatter.format(formatString, args: _*)
          
          val result1 = buffer.toString
          val index = result1.indexOf('.')
          val result = result1.replace(".", "")

          val array = result.toCharArray.map{ char => RValue(char.toByte, new CBasicType(IBasicType.Kind.eChar, 0))}

          state.setValue(index, decpt)

          state.setArray(array, buf, 1)
          Some(buf)
        }
      }
}