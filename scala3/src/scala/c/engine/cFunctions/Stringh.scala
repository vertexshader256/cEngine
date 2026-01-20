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
				Some(if (theChar.isLetter) 1 else 0)
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Char]("isdigit") {
			def func(theChar: Char) = {
				Some(if (theChar.isDigit) 1 else 0)
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Char]("isxdigit") {
			def func(theChar: Char) = {
				Some(if (theChar.toString.matches("^[0-9a-fA-F]+$")) 1 else 0)
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Char]("tolower") {
			def func(theChar: Char) = {
				Some(theChar.toLower.toByte)
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Char]("toupper") {
			def func(theChar: Char) = {
				Some(theChar.toUpper.toByte)
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Char]("isupper") {
			def func(theChar: Char) = {
				Some(if (theChar.isUpper) 1 else 0)
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Char]("isspace") {
			def func(theChar: Char) = {
				Some(if (theChar.isSpaceChar || theChar.toInt == 13 || theChar.toInt == 10) 1 else 0)
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
				Some(len)
			}
		}.generate

		scalaFunctions += new TwoParameterFunction[Address, Int]("strchr") {
			def func(stringAddress: Address, char: Int) = {
				val str = Utils.readString(stringAddress)
				val offset = str.indexOf(char.toChar)

				if offset != -1 then
					Some(stringAddress.location + offset)
				else
					Some(0)
			}
		}.generate

		scalaFunctions += new ThreeParameterFunction[Address, Address, Int]("strncpy") {
			def func(dst: Address, src: Address, numBytes: Int) = {
				val str1 = Utils.readString(src)(using state)
				state.copy(dst, src, Math.min(str1.length + 1, numBytes))
				None
			}
		}.generate

		scalaFunctions += new TwoParameterFunction[Address, Address]("strtok") {
			def func(source: Address, delim: Address) = {
				val delimiters = Utils.readString(delim)(using state)
				val sourceAddr = source.location

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
							Some(0)
						else {
							val retVal = strTokPosition.location
							strTokPosition = strTokPosition + initialStr.length
							Some(retVal)
						} else
						Some(firstNonToken.location)
				} else {
					var sourceStr = Utils.readString(Address(sourceAddr))(using state)

					var tokenFound = false
					var doneFindingTokens = false

					sourceStr.zipWithIndex.foreach { (char, index) =>
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

					Some(sourceAddr)
				}
			}
		}.generate

		scalaFunctions += new TwoParameterFunction[Address, Address]("strcpy") {
			def func(dst: Address, src: Address) = {
				val str1 = Utils.readString(src)
				state.copy(dst, src, str1.length + 1)
				None
			}
		}.generate

		scalaFunctions += new TwoParameterFunction[Address, Address]("strcmp") {
			def func(straddy: Address, straddy2: Address) = {
				val str1 = Utils.readString(straddy)
				val str2 = Utils.readString(straddy2)
				val same = str1 == str2
				Some((if (same) 0 else 1))
			}
		}.generate

		scalaFunctions += new TwoParameterFunction[Address, Address]("strcat") {
			def func(dst: Address, src: Address) = {
				val str1 = Utils.readString(dst)
				val str2 = Utils.readString(src)

				val concat = str1 + str2 + "\u0000"
				val bytes = concat.getBytes
				state.stack.writeDataBlock(bytes, dst.location)
				Some(dst.location) // returns a pointer to the destination string
			}
		}.generate

		// offsetof is actually a macro, but this way works
		scalaFunctions += new TwoParameterFunction[Address, Address]("offsetof") {
			def func(structNameAddr: Address, memberNameAddr: Address) = {
				val memberName = Utils.readString(memberNameAddr)
				val stuctName = Utils.readString(structNameAddr)

				val struct = state.structs.find { x => ("struct " + x.getName) == stuctName }.get

				Some(Structures.offsetof(struct, memberName, state))
			}
		}.generate

		scalaFunctions += new TwoParameterFunction[Address, Address]("strstr") {
			def func(haystackAddr: Address, needleAddr: Address) = {
				val needle = Utils.readString(needleAddr)
				val haystack = Utils.readString(haystackAddr)

				val indexOf = haystack.indexOf(needle)

				if indexOf == -1 then
					Some(0) // null pointer
				else if needle.isEmpty then
					Some(haystackAddr.location)
				else
					Some(haystackAddr.location + indexOf)
			}
		}.generate

		scalaFunctions += new ThreeParameterFunction[Address, Address, Int]("memcmp") {
			def func(memaddy2: Address, memaddy: Address, numBytes: Int) = {
				var same = true

				for (i <- (0 until numBytes)) {

					val value = state.stack.readFromMemoryRaw(TypeHelper.charType, memaddy.location + i)
					val value1 = TypeHelper.castSign(TypeHelper.charType, value).value

					val value2 = state.stack.readFromMemoryRaw(CBasicType(IBasicType.Kind.eChar, 0), memaddy2.location + i)
					val value3 = TypeHelper.castSign(TypeHelper.charType, value2).value

					same &= value1 == value3
				}

				Some((if (same) 0 else 1))
			}
		}.generate
	}
}
