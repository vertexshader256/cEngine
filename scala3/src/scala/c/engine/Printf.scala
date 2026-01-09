package scala.c.engine

import java.util.{Formatter, Locale}
import scala.collection.mutable.ListBuffer

// function which I use to simulate C's standard printf()
object Printf {

	def convertBoolean2(num: RValue): Object = {
		val x = num.value
		val convertedBool = x match
			case bool: Boolean => if bool then 1 else 0
			case _ => x

		convertedBool.asInstanceOf[Object]
	}

	private def printChar(theValue: RValue) = {
		val buffer2 = new StringBuffer()
		val formatter2 = new Formatter(buffer2, Locale.US)
		val value = theValue.value.asInstanceOf[Object]
		formatter2.format("%c", List(value) *)
		buffer2.toString
	}

	private def printString(stringFormat: String, theValue: RValue)(using State) = {
		val formatString = stringFormat
		val buffer2 = new StringBuffer()
		val formatter2 = new Formatter(buffer2, Locale.US)

		val theVal = theValue.value
		val stringAddr = theVal match
			case int: Int => int
			case long: Long => long.toInt

		val value = if stringAddr != 0 then
			val str = Utils.readString(stringAddr)
			str.split(System.lineSeparator()).mkString.asInstanceOf[Object]
		else
			"(null)".asInstanceOf[Object]

		formatter2.format("%" + formatString + "s", List(value) *)
		buffer2.toString
	}

	private def printHex(stringFormat: String, value: RValue) = {

		val buffer2 = new StringBuffer()
		val formatter2 = new Formatter(buffer2, Locale.US)
		val resolved = new ListBuffer[Object]()

		val x = value.value

		val converted = x match
			case long: Long => long & 0xFFFFFFFFL
			case _ => TypeHelper.castToUnsigned(false, x)

		resolved += converted.asInstanceOf[Object]

		if stringFormat.contains("x") then
			formatter2.format("%x", resolved.toSeq *)
		else
			formatter2.format("%X", resolved.toSeq *)

		var result = if stringFormat == "#x" then
			"0x" + buffer2.toString
		else if stringFormat == "#X" then
			"0X" + buffer2.toString
		else
			buffer2.toString

		var wasPaddingNumFound = false
		stringFormat.take(2).mkString.toIntOption.foreach: intVal =>
			wasPaddingNumFound = true
			if intVal <= result.length then
				result
			else
				val diff = intVal - result.length
				(0 until diff).foreach{ _ => result = "0" + result}

		if !wasPaddingNumFound then {
			// if it wasn't found with 2 numbers, try one
			stringFormat.take(1).mkString.toIntOption.foreach: intVal =>
				wasPaddingNumFound = true
				if intVal <= result.length then
					result
				else
					val diff = intVal - result.length
					(0 until diff).foreach{ _ => result = "0" + result}
		}

		result
	}

	private def printUnsigned(stringFormat: String, value: RValue) = {

		val buffer2 = new StringBuffer()
		val formatter2 = new Formatter(buffer2, Locale.US)
		val resolved = new ListBuffer[Object]()

		val x = value.value

		val converted = x match
			case long: Long => long & 0xFFFFFFFFL
			case _ => TypeHelper.castToUnsigned(false, x)

		resolved += converted.asInstanceOf[Object]

		formatter2.format("%d", resolved.toSeq *)
		buffer2.toString
	}

	private def printLongLongUnsigned(stringFormat: String, value: RValue) = {
		val buffer2 = new StringBuffer()
		val formatter2 = new Formatter(buffer2, Locale.US)
		val resolved = new ListBuffer[Object]()

		val bigInt = value.value
		val longVal = Long.box(bigInt.asInstanceOf[Long])

		resolved += java.lang.Long.toUnsignedString(Long.box(longVal))

		formatter2.format("%s", resolved.toSeq *)
		buffer2.toString
	}

	private def printDeciminal(stringFormat: String, value: RValue, convertToInt: Boolean) = {
		var currentFormatString = stringFormat
		val buffer2 = new StringBuffer()
		val formatter2 = new Formatter(buffer2, Locale.US)
		val resolved = new ListBuffer[Object]()

		val num = value.value
		currentFormatString += 'd'

		num match
			case long: Long if convertToInt => resolved += Int.box(long.toInt)
			case _ => resolved += convertBoolean2(value)

		formatter2.format("%" + currentFormatString, resolved.toSeq *)
		buffer2.toString
	}

	private def printFloat(formatString: String, base: Object): String = {
		var currentFormatString = formatString
		var buffer2 = new StringBuffer()
		var formatter2 = new Formatter(buffer2, Locale.US)
		val resolved = new ListBuffer[Object]()
		val output = new StringBuffer()
		val convert = base match
			case boolean: java.lang.Boolean =>
				if boolean then 1.0 else 0.0
			case int: java.lang.Integer =>
				int.toFloat
			case _ =>
				base
		currentFormatString += 'f'

		formatter2.format("%" + currentFormatString, List(convert) *)

		var format: String = ""
		if (buffer2.toString.contains("Infinity") || buffer2.toString.contains("NaN")) {
			currentFormatString = "s"
			resolved += buffer2.toString.replace("Infinity", "inf").replace("NaN", "-nan(ind)")
		} else {
			base match
				case boolean: java.lang.Boolean =>
					val converted = if boolean then 1.0f else 0.0f
					resolved += Float.box(converted)
				case int: java.lang.Integer =>
					resolved += Float.box(int.toFloat)
				case _ =>
					resolved += base
		}
		buffer2 = new StringBuffer()
		formatter2 = new Formatter(buffer2, Locale.US)
		formatter2.format("%" + currentFormatString, resolved.toSeq *)
		output.append(buffer2.toString)

		output.toString
	}

	def printf(formattedOutputParams: Array[RValue], state: State): String = {
		val str = Utils.readString(formattedOutputParams.last.value.asInstanceOf[Int])(using state)

		var percentFound = false
		var paramCount = 0

		val varArgs = formattedOutputParams.reverse.tail.toList

		def convertBoolean(): Object = {
			val x = TypeHelper.toRValue(varArgs(paramCount))(using state).value
			val convertedBool = x match
				case bool: Boolean => if bool then 1 else 0
				case _ => x

			convertedBool.asInstanceOf[Object]
		}

		var isDone = false
		var remainder = str.toCharArray
		val output = new StringBuffer()

		var currentChar = remainder.headOption

		if remainder.isEmpty then
			isDone = true

		while (!isDone) {
			var currentChar = remainder.headOption.get
			remainder = remainder.drop(1)

			if (currentChar == '%') {
				var currentFormatString = ""
				var wasFormatStringFound = false
				var charsToDrop: Int = 0
				var charsToOutput = ""

				while (!wasFormatStringFound) {

					def variable = TypeHelper.toRValue(varArgs(paramCount))(using state)

					case class OutputFormat(identifier: String, toText: (String, RValue) => String)

//					val formats: Seq[OutputFormat] = Seq(
//						OutputFormat("f", (format, rValue) => printFloat(currentFormatString, rValue.value.asInstanceOf[Object])),
//						OutputFormat("hd", (format, rValue) => printDeciminal(currentFormatString, rValue, true)),
//						OutputFormat("d", (format, rValue) => printDeciminal(currentFormatString, rValue, true)),
//						OutputFormat("u", (format, rValue) => printUnsigned(currentFormatString, rValue)),
//						OutputFormat("llu", (format, rValue) => printLongLongUnsigned(currentFormatString, rValue)),
//						OutputFormat("ld", (format, rValue) => printDeciminal(currentFormatString, rValue, true)),
//						OutputFormat("lld", (format, rValue) => printDeciminal("", rValue, false)),
//						OutputFormat("s", (format, rValue) => printDeciminal("", rValue, false)),
//					)
//
//					formats.find{ format =>
//						remainder.startsWith(format.identifier)
//					}.foreach{ format =>
//						charsToDrop = format.identifier.length
//						charsToOutput = format.toText(currentFormatString, variable)
//					}

					if (remainder.startsWith("f")) {
						val theType = variable.value.asInstanceOf[Object]
						val floatOutput = printFloat(currentFormatString, theType)
						charsToOutput = floatOutput
						charsToDrop = 1
					} else if (remainder.startsWith("hd")) {
						charsToOutput = printDeciminal(currentFormatString, variable, true)
						charsToDrop = 2
					} else if (remainder.startsWith("d")) {
						charsToOutput = printDeciminal(currentFormatString, variable, true)
						charsToDrop = 1
					} else if (remainder.startsWith("u")) { // unsigned
						charsToOutput = printUnsigned(currentFormatString, variable)
						charsToDrop = 1
					} else if (remainder.startsWith("llu")) { // long long unsigned
						charsToOutput = printLongLongUnsigned(currentFormatString, variable)
						charsToDrop = 3
					} else if (remainder.startsWith("ld")) {
						charsToOutput = printDeciminal(currentFormatString, variable, true)
						charsToDrop = 2
					} else if (remainder.startsWith("lld")) {
						charsToOutput = printDeciminal("", variable, false)
						charsToDrop = 3
					} else if (remainder.startsWith("s")) {
						charsToOutput = printString(currentFormatString, variable)(using state)
						charsToDrop = 1
					} else if (remainder.startsWith("c")) {
						charsToOutput = printChar(variable)
						charsToDrop = 1
					} else if (remainder.startsWith("#x") || remainder.startsWith("#X")) {
						charsToOutput = printHex(remainder.take(2).mkString, variable)
						charsToDrop = 2
					} else if (remainder.startsWith("x") || remainder.startsWith("X")) {
						currentFormatString += currentChar + remainder.head.toString
						charsToOutput = printHex(currentFormatString, variable)
						charsToDrop = 1
					} else if (remainder.startsWith("p")) {
						charsToOutput = printHex("16X", variable)
						charsToDrop = 1
					} else {
						currentChar = remainder.headOption.getOrElse('_')
						remainder = remainder.drop(1)
						currentFormatString += currentChar
					}

					if charsToDrop != 0 then
						paramCount += 1
						remainder = remainder.drop(charsToDrop)
						wasFormatStringFound = true
						output.append(charsToOutput)
				} // while
			} else {

				// convert tabs "\t" string to '\t' character
				if (remainder.headOption.contains('t') && currentChar == '\\') {
					output.append("\t")
					currentChar = remainder.headOption.getOrElse('_')
					remainder = remainder.drop(1)
				} else {
					output.append(currentChar)
				}
			}

			if (remainder.isEmpty) {
				isDone = true
			}
		}

		output.toString
	}
}
