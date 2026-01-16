package scala.c.engine.cFunctions

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import java.io.File
import java.util.{Formatter, Locale}
import scala.c.engine.*
import scala.c.engine.models.*
import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.c.engine.models.Function

object Stringh {

	def addFunctions(scalaFunctions: ListBuffer[Function]) = {
		/////////////////////////////////////////////////////////////////
		//                  <string.h> functions                       //
		/////////////////////////////////////////////////////////////////

		scalaFunctions += new EmulatedFunction("isalpha") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val theChar = formattedOutputParams.head.value.asInstanceOf[char].toChar
				Some(RValue(if (theChar.isLetter) 1 else 0))
			}
		}

		scalaFunctions += new EmulatedFunction("isdigit") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val theChar = formattedOutputParams.head.value match {
					case c: char => c.toChar
					case int: Int => int.toChar
				}
				Some(RValue(if (theChar.isDigit) 1 else 0))
			}
		}

		scalaFunctions += new EmulatedFunction("isxdigit") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val theChar = formattedOutputParams.head.value match {
					case c: char => c.toChar
					case int: Int => int.toChar
				}
				Some(RValue(if (theChar.toString.matches("^[0-9a-fA-F]+$")) 1 else 0))
			}
		}

		scalaFunctions += new EmulatedFunction("tolower") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val theChar = formattedOutputParams.head.value match {
					case c: char => c.toChar
					case int: Int => int.toChar
				}
				Some(RValue(theChar.toLower.toByte))
			}
		}

		scalaFunctions += new EmulatedFunction("toupper") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val theChar = formattedOutputParams.head.value.asInstanceOf[char].toChar
				Some(RValue(theChar.toUpper.toByte))
			}
		}

		scalaFunctions += new EmulatedFunction("isupper") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val theChar = formattedOutputParams.head.value match {
					case int: int => int.toChar
					case char: char => char.toChar
				}
				Some(RValue(if (theChar.isUpper) 1 else 0))
			}
		}

		scalaFunctions += new EmulatedFunction("isspace") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val theChar = formattedOutputParams.head.value match {
					case c: char => c.toChar
					case int: Int => int.toChar
				}
				Some(RValue(if (theChar.isSpaceChar || theChar.toInt == 13 || theChar.toInt == 10) 1 else 0))
			}
		}

		scalaFunctions += new EmulatedFunction("memmove") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val dst = formattedOutputParams(0).value.asInstanceOf[Int]
				val src = formattedOutputParams(1).value.asInstanceOf[Int]
				val numBytes = formattedOutputParams(2).value.asInstanceOf[Int]

				state.copy(Address(dst, state.stack), Address(src, state.stack), numBytes)
				None
			}
		}

		scalaFunctions += new EmulatedFunction("memcpy") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val numBytes = formattedOutputParams(0).value match {
					case int: Int => int
					case long: Long => long.toInt
				}
				val src = formattedOutputParams(1).value.asInstanceOf[Int]
				val dst = formattedOutputParams(2).value.asInstanceOf[Int]

				state.copy(Address(dst, state.stack), Address(src, state.stack), numBytes)
				None
			}
		}

		scalaFunctions += new EmulatedFunction("memset") {
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

		scalaFunctions += new EmulatedFunction("strlen") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val straddy = formattedOutputParams.head.value match {
					//case AddressInfo(addr, _) => addr.value
					case int: Int => int
				}
				var current: char = 0
				var i = 0

				val value = state.stack.readFromMemoryRaw(TypeHelper.charType, straddy + i)
				current = TypeHelper.castSign(TypeHelper.charType, value).value.asInstanceOf[char]

				while (current != 0) {
					if (current != 0) {
						i += 1
					}
					val value = state.stack.readFromMemoryRaw(TypeHelper.charType, straddy + i)
					current = TypeHelper.castSign(TypeHelper.charType, value).value.asInstanceOf[char]
				}
				Some(RValue(i))
			}
		}

		scalaFunctions += new EmulatedFunction("strchr") {
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

		scalaFunctions += new EmulatedFunction("strncpy") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val num = formattedOutputParams(0).value.asInstanceOf[Int]
				val src = formattedOutputParams(1).value.asInstanceOf[Int]
				val dst = formattedOutputParams(2).value.asInstanceOf[Int]

				val str1 = Utils.readString(src)(using state)

				state.copy(Address(dst, state.stack), Address(src, state.stack), Math.min(str1.length + 1, num))
				None
			}
		}

		scalaFunctions += new EmulatedFunction("strcpy") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val src = formattedOutputParams(0).value.asInstanceOf[Int]
				val dst = formattedOutputParams(1).value.asInstanceOf[Int]

				val str1 = Utils.readString(src)(using state)

				state.copy(Address(dst, state.stack), Address(src, state.stack), str1.length + 1)
				None
			}
		}

		scalaFunctions += new EmulatedFunction("strcmp") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val straddy = formattedOutputParams(0).value.asInstanceOf[Int]
				val straddy2 = formattedOutputParams(1).value.asInstanceOf[Int]

				val str1 = Utils.readString(straddy)(using state)
				val str2 = Utils.readString(straddy2)(using state)

				val same = str1 == str2
				Some(RValue((if (same) 0 else 1)))
			}
		}

		scalaFunctions += new EmulatedFunction("strcat") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val dstAddr = formattedOutputParams(1).value.asInstanceOf[Int]
				val stringToAppendAddr = formattedOutputParams(0).value.asInstanceOf[Int]

				val str1 = Utils.readString(dstAddr)(using state)
				val str2 = Utils.readString(stringToAppendAddr)(using state)

				val concat = str1 + str2 + "\u0000"
				val bytes = concat.getBytes
				state.stack.writeDataBlock(bytes, dstAddr)
				Some(formattedOutputParams(0)) // returns a pointer to the destination string
			}
		}

		scalaFunctions += new EmulatedFunction("offsetof") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val straddy = formattedOutputParams(0).value.asInstanceOf[Int]
				val straddy2 = formattedOutputParams(1).value.asInstanceOf[Int]

				val memberName = Utils.readString(straddy)(using state)
				val stuctName = Utils.readString(straddy2)(using state)

				val struct = state.structs.find { x => ("struct " + x.getName) == stuctName }.get

				Some(RValue(Structures.offsetof(struct, memberName, state)))
			}
		}

		scalaFunctions += new EmulatedFunction("memcmp") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val numBytes = formattedOutputParams(0).value match {
					case long: Long => long.toInt
					case int: Int => int
				}
				val memaddy = formattedOutputParams(1).value.asInstanceOf[int]
				val memaddy2 = formattedOutputParams(2).value.asInstanceOf[int]

				var same = true

				for (i <- (0 until numBytes)) {

					val value = state.stack.readFromMemoryRaw(TypeHelper.charType, memaddy + i)
					val value1 = TypeHelper.castSign(TypeHelper.charType, value).value

					val value2 = state.stack.readFromMemoryRaw(CBasicType(IBasicType.Kind.eChar, 0), memaddy2 + i)
					val value3 = TypeHelper.castSign(TypeHelper.charType, value2).value

					same &= value1 == value3
				}

				Some(RValue((if (same) 0 else 1)))
			}
		}
	}
}
