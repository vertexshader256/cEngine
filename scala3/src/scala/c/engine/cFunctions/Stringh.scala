package scala.c.engine.cFunctions

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import java.io.File
import java.util.{Formatter, Locale}
import scala.c.engine.*
import scala.c.engine.models.*
import scala.collection.mutable.ListBuffer
import scala.util.Try

object Stringh {

	def addFunctions(scalaFunctions: ListBuffer[Function]) = {
		/////////////////////////////////////////////////////////////////
		//                  <string.h> functions                       //
		/////////////////////////////////////////////////////////////////

		scalaFunctions += new Function("isalpha", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val theChar = formattedOutputParams.head.value.asInstanceOf[char].toChar
				Some(RValue(if (theChar.isLetter) 1 else 0))
			}
		}

		scalaFunctions += new Function("isdigit", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val theChar = formattedOutputParams.head.value match {
					case c: char => c.toChar
					case int: Int => int.toChar
				}
				Some(RValue(if (theChar.isDigit) 1 else 0))
			}
		}

		scalaFunctions += new Function("isxdigit", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val theChar = formattedOutputParams.head.value match {
					case c: char => c.toChar
					case int: Int => int.toChar
				}
				Some(RValue(if (theChar.toString.matches("^[0-9a-fA-F]+$")) 1 else 0))
			}
		}

		scalaFunctions += new Function("tolower", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val theChar = formattedOutputParams.head.value match {
					case c: char => c.toChar
					case int: Int => int.toChar
				}
				Some(RValue(theChar.toLower.toByte))
			}
		}

		scalaFunctions += new Function("toupper", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val theChar = formattedOutputParams.head.value.asInstanceOf[char].toChar
				Some(RValue(theChar.toUpper.toByte))
			}
		}

		scalaFunctions += new Function("isupper", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val theChar = formattedOutputParams.head.value match {
					case int: int => int.toChar
					case char: char => char.toChar
				}
				Some(RValue(if (theChar.isUpper) 1 else 0))
			}
		}

		scalaFunctions += new Function("isspace", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val theChar = formattedOutputParams.head.value match {
					case c: char => c.toChar
					case int: Int => int.toChar
				}
				Some(RValue(if (theChar.isSpaceChar || theChar.toInt == 13 || theChar.toInt == 10) 1 else 0))
			}
		}

		scalaFunctions += new Function("memmove", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val dst = formattedOutputParams(0).value.asInstanceOf[Int]
				val src = formattedOutputParams(1).value.asInstanceOf[Int]
				val numBytes = formattedOutputParams(2).value.asInstanceOf[Int]

				state.copy(dst, src, numBytes)
				None
			}
		}

		scalaFunctions += new Function("memcpy", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
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

		scalaFunctions += new Function("memset", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val numBytes = formattedOutputParams(0).value match {
					case int: Int => int
					case long: Long => long.toInt
				}
				val value = formattedOutputParams(1).value.asInstanceOf[Int].toByte
				val dst = formattedOutputParams(2).value.asInstanceOf[Int]

				state.set(dst, value, numBytes)
				None
			}
		}

		scalaFunctions += new Function("strlen", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val straddy = formattedOutputParams.head.value match {
					//case AddressInfo(addr, _) => addr.value
					case int: Int => int
				}
				var current: char = 0
				var i = 0

				val value = state.Stack.readFromMemoryRaw(TypeHelper.charType, straddy + i)
				current = TypeHelper.castSign(TypeHelper.charType, value).value.asInstanceOf[char]

				while (current != 0) {
					if (current != 0) {
						i += 1
					}
					val value = state.Stack.readFromMemoryRaw(TypeHelper.charType, straddy + i)
					current = TypeHelper.castSign(TypeHelper.charType, value).value.asInstanceOf[char]
				}
				Some(RValue(i))
			}
		}

		scalaFunctions += new Function("strchr", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val char = formattedOutputParams(0).value match {
					case int: Int => int
					case byte: Byte => byte.toInt
				}
				val straddy = formattedOutputParams(1).value.asInstanceOf[Int]

				val str = Utils.readString(straddy)(using state)

				val offset = str.indexOf(char.toChar)

				if (offset != -1) {
					Some(RValue(straddy + offset))
				} else {
					Some(RValue(0))
				}
			}
		}

		scalaFunctions += new Function("strncpy", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val num = formattedOutputParams(0).value.asInstanceOf[Int]
				val src = formattedOutputParams(1).value.asInstanceOf[Int]
				val dst = formattedOutputParams(2).value.asInstanceOf[Int]

				val str1 = Utils.readString(src)(using state)

				state.copy(dst, src, Math.min(str1.length + 1, num))
				None
			}
		}

		scalaFunctions += new Function("strcpy", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val src = formattedOutputParams(0).value.asInstanceOf[Int]
				val dst = formattedOutputParams(1).value.asInstanceOf[Int]

				val str1 = Utils.readString(src)(using state)

				state.copy(dst, src, str1.length + 1)
				None
			}
		}

		scalaFunctions += new Function("strcmp", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val straddy = formattedOutputParams(0).value.asInstanceOf[Int]
				val straddy2 = formattedOutputParams(1).value.asInstanceOf[Int]

				val str1 = Utils.readString(straddy)(using state)
				val str2 = Utils.readString(straddy2)(using state)

				val same = str1 == str2
				Some(RValue((if (same) 0 else 1)))
			}
		}

		scalaFunctions += new Function("strcat", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val dstAddr = formattedOutputParams(1).value.asInstanceOf[Int]
				val stringToAppendAddr = formattedOutputParams(0).value.asInstanceOf[Int]

				val str1 = Utils.readString(dstAddr)(using state)
				val str2 = Utils.readString(stringToAppendAddr)(using state)

				val concat = str1 + str2 + "\u0000"
				val bytes = concat.getBytes
				state.Stack.tape.writeDataBlock(bytes, dstAddr)
				Some(formattedOutputParams(0)) // returns a pointer to the destination string
			}
		}

		scalaFunctions += new Function("offsetof", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val straddy = formattedOutputParams(0).value.asInstanceOf[Int]
				val straddy2 = formattedOutputParams(1).value.asInstanceOf[Int]

				val memberName = Utils.readString(straddy)(using state)
				val stuctName = Utils.readString(straddy2)(using state)

				val struct = state.structs.find { x => ("struct " + x.getName) == stuctName }.get

				Some(RValue(Structures.offsetof(struct, memberName, state)))
			}
		}

		scalaFunctions += new Function("memcmp", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val numBytes = formattedOutputParams(0).value match {
					case long: Long => long.toInt
					case int: Int => int
				}
				val memaddy = formattedOutputParams(1).value.asInstanceOf[int]
				val memaddy2 = formattedOutputParams(2).value.asInstanceOf[int]

				var same = true

				for (i <- (0 until numBytes)) {

					val value = state.Stack.readFromMemoryRaw(TypeHelper.charType, memaddy + i)
					val value1 = TypeHelper.castSign(TypeHelper.charType, value).value

					val value2 = state.Stack.readFromMemoryRaw(CBasicType(IBasicType.Kind.eChar, 0), memaddy2 + i)
					val value3 = TypeHelper.castSign(TypeHelper.charType, value2).value

					same &= value1 == value3
				}

				Some(RValue((if (same) 0 else 1)))
			}
		}
	}
}
