package scala.c.engine.cFunctions

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import scala.c.engine.*
import scala.c.engine.models.{Function, *}
import scala.collection.mutable.ListBuffer

object Stringh {

	private var strTokPosition = Address(0)

	def addFunctions(scalaFunctions: ListBuffer[Function])(implicit theState: State) = {
		/////////////////////////////////////////////////////////////////
		//                  <string.h> functions                       //
		/////////////////////////////////////////////////////////////////

		scalaFunctions += new OneParameterFunction[Char]("isalpha") {
			def func(theChar: Char) = {
				Some(RValue(if (theChar.isLetter) 1 else 0))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Char]("isdigit") {
			def func(theChar: Char) = {
				Some(RValue(if (theChar.isDigit) 1 else 0))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Char]("isxdigit") {
			def func(theChar: Char) = {
				Some(RValue(if (theChar.toString.matches("^[0-9a-fA-F]+$")) 1 else 0))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Char]("tolower") {
			def func(theChar: Char) = {
				Some(RValue(theChar.toLower.toByte))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Char]("toupper") {
			def func(theChar: Char) = {
				Some(RValue(theChar.toUpper.toByte))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Char]("isupper") {
			def func(theChar: Char) = {
				Some(RValue(if (theChar.isUpper) 1 else 0))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Char]("isspace") {
			def func(theChar: Char) = {
				Some(RValue(if (theChar.isSpaceChar || theChar.toInt == 13 || theChar.toInt == 10) 1 else 0))
			}
		}.generate

		scalaFunctions += new ThreeParameterFunction[Address, Address, Int]("memmove") {
			def func(dst: Address, src: Address, numBytes: Int) = {
				state.copy(dst, src, numBytes)
				None
			}
		}.generate

		scalaFunctions += new ThreeParameterFunction[Address, Address, Int]("memcpy") {
			def func(dst: Address, src: Address, numBytes: Int) = {
				state.copy(dst, src, numBytes)
				None
			}
		}.generate

		scalaFunctions += new ThreeParameterFunction[Address, Byte, Int]("memset") {
			def func(dst: Address, filledBy: Byte, numBytes: Int) = {
				state.set(dst, filledBy, numBytes)
				None
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Address]("strlen") {
			def func(str: Address) = {
				val len = Utils.readString(str).length
				Some(RValue(len))
			}
		}.generate

		scalaFunctions += new TwoParameterFunction[Address, Int]("strchr") {
			def func(stringAddress: Address, char: Int) = {
				val str = Utils.readString(stringAddress)
				val offset = str.indexOf(char.toChar)

				if offset != -1 then
					Some(RValue(stringAddress.location + offset))
				else
					Some(RValue(0))
			}
		}.generate

		scalaFunctions += new EmulatedFunction("strncpy") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val num = formattedOutputParams(0).value match
					case int: Int => int
					case long: Long => long.toInt
				val src = formattedOutputParams(1).value.asInstanceOf[Int]
				val dst = formattedOutputParams(2).value.asInstanceOf[Int]

				val str1 = Utils.readString(Address(src))(using state)

				state.copy(Address(dst), Address(src), Math.min(str1.length + 1, num))
				None
			}
		}

		scalaFunctions += new EmulatedFunction("strtok") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val delim = formattedOutputParams(0).value.asInstanceOf[Int]
				val sourceAddr = formattedOutputParams(1).value.asInstanceOf[Int]
				val delimiters = Utils.readString(Address(delim))(using state)

				if (sourceAddr == 0) { // a repeated call
					var initialStr = Utils.readString(strTokPosition)(using state)

					var nonTokenFound = false
					val initialAddress = strTokPosition
					var hasToken = false

					initialStr.zipWithIndex.foreach { (char, index) =>
						if !nonTokenFound then
							if delimiters.contains(char) then
								hasToken = true
								initialStr = initialStr.updated(index, '\u0000')
								strTokPosition = Address(initialAddress.location + index + 1)
							else
								nonTokenFound = true
					}

					val firstNonToken = strTokPosition
					var sourceStr = Utils.readString(strTokPosition)(using state)

					var tokenFound = false
					var doneFindingTokens = false

					sourceStr.zipWithIndex.foreach { (char, index) =>
						if !doneFindingTokens then
							if delimiters.contains(char) then
								hasToken = true
								tokenFound = true
								sourceStr = sourceStr.updated(index, '\u0000')
								strTokPosition = Address(firstNonToken.location + index + 1)
								state.stack.writeDataBlock(sourceStr.getBytes, firstNonToken.location)
							else if tokenFound then
								doneFindingTokens = true
					}

					if !hasToken then
						if initialStr.isEmpty then
							Some(RValue(0))
						else {
							val retVal = RValue(strTokPosition.location)
							strTokPosition = strTokPosition + initialStr.length
							Some(retVal)
						} else
						Some(RValue(firstNonToken.location))
				} else {
					var sourceStr = Utils.readString(Address(sourceAddr))(using state)

					var tokenFound = false
					var doneFindingTokens = false

					sourceStr.zipWithIndex.foreach{ (char, index) =>
						if !doneFindingTokens then
							if delimiters.contains(char) then
								tokenFound = true
								sourceStr = sourceStr.updated(index, '\u0000')
								strTokPosition = Address(sourceAddr + index + 1)
								state.stack.writeDataBlock(sourceStr.getBytes, sourceAddr)
								doneFindingTokens = true
							else if tokenFound then
								doneFindingTokens = true
					}

					Some(RValue(sourceAddr))
				}
			}
		}

		scalaFunctions += new EmulatedFunction("strcpy") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val src = formattedOutputParams(0).value.asInstanceOf[Int]
				val dst = formattedOutputParams(1).value.asInstanceOf[Int]

				val str1 = Utils.readString(Address(src))(using state)

				state.copy(Address(dst), Address(src), str1.length + 1)
				None
			}
		}

		scalaFunctions += new EmulatedFunction("strcmp") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val straddy = formattedOutputParams(0).value.asInstanceOf[Int]
				val straddy2 = formattedOutputParams(1).value.asInstanceOf[Int]

				val str1 = Utils.readString(Address(straddy))(using state)
				val str2 = Utils.readString(Address(straddy2))(using state)

				val same = str1 == str2
				Some(RValue((if (same) 0 else 1)))
			}
		}

		scalaFunctions += new EmulatedFunction("strcat") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val dstAddr = formattedOutputParams(1).value.asInstanceOf[Int]
				val stringToAppendAddr = formattedOutputParams(0).value.asInstanceOf[Int]

				val str1 = Utils.readString(Address(dstAddr))(using state)
				val str2 = Utils.readString(Address(stringToAppendAddr))(using state)

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

				val memberName = Utils.readString(Address(straddy))(using state)
				val stuctName = Utils.readString(Address(straddy2))(using state)

				val struct = state.structs.find { x => ("struct " + x.getName) == stuctName }.get

				Some(RValue(Structures.offsetof(struct, memberName, state)))
			}
		}

		scalaFunctions += new EmulatedFunction("strstr") { // indexof equivilent
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val straddy = formattedOutputParams(0).value.asInstanceOf[Int] // needle
				val straddy2 = formattedOutputParams(1).value.asInstanceOf[Int] // haystack

				val needle = Utils.readString(Address(straddy))(using state)
				val haystack = Utils.readString(Address(straddy2))(using state)

				val indexOf = haystack.indexOf(needle)

				if indexOf == -1 then
					Some(RValue(0)) // null pointer
				else if needle.isEmpty then
					Some(RValue(straddy2))
				else
					Some(RValue(straddy2 + indexOf))
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
