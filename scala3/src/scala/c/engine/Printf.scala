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
		val stringAddr = theVal.asInstanceOf[Int]

		val value = if stringAddr != 0 then
			val str = Utils.readString(stringAddr)
			str.split(System.lineSeparator()).mkString.asInstanceOf[Object]
		else
			"(null)".asInstanceOf[Object]

		formatter2.format("%" + formatString + "s", List(value) *)
		buffer2
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

				while (!wasFormatStringFound) {
					if (remainder.startsWith("f")) {
						val theType = TypeHelper.toRValue(varArgs(paramCount))(using state).value.asInstanceOf[Object]
						val floatOutput = printFloat(currentFormatString, theType)
						output.append(floatOutput)
						paramCount += 1
						remainder = remainder.drop(1)
						wasFormatStringFound = true
					} else if (remainder.startsWith("hd")) {
						output.append(printDeciminal(currentFormatString, TypeHelper.toRValue(varArgs(paramCount))(using state), true))
						paramCount += 1
						remainder = remainder.drop(2)
						wasFormatStringFound = true
					} else if (remainder.startsWith("d")) {
						output.append(printDeciminal(currentFormatString, TypeHelper.toRValue(varArgs(paramCount))(using state), true))
						paramCount += 1
						remainder = remainder.drop(1)
						wasFormatStringFound = true
					} else if (remainder.startsWith("u")) { // unsigned
						output.append(printUnsigned(currentFormatString, TypeHelper.toRValue(varArgs(paramCount))(using state)))
						paramCount += 1
						remainder = remainder.drop(1)
						wasFormatStringFound = true
					} else if (remainder.startsWith("llu")) { // long long unsigned
						output.append(printLongLongUnsigned(currentFormatString, TypeHelper.toRValue(varArgs(paramCount))(using state)))
						paramCount += 1
						remainder = remainder.drop(3)
						wasFormatStringFound = true
					} else if (remainder.startsWith("ld")) {
						output.append(printDeciminal(currentFormatString, TypeHelper.toRValue(varArgs(paramCount))(using state), true))
						remainder = remainder.drop(2)
						paramCount += 1
						wasFormatStringFound = true
					} else if (remainder.startsWith("lld")) {
						output.append(printDeciminal("", TypeHelper.toRValue(varArgs(paramCount))(using state), false))
						remainder = remainder.drop(3)
						paramCount += 1
						wasFormatStringFound = true
					} else if (remainder.startsWith("s")) {
						output.append(printString(currentFormatString, varArgs(paramCount))(using state))
						remainder = remainder.drop(1)
						paramCount += 1
						wasFormatStringFound = true
					} else if (remainder.startsWith("c")) {
						output.append(printChar(TypeHelper.toRValue(varArgs(paramCount))(using state)))
						remainder = remainder.drop(1)
						paramCount += 1
						wasFormatStringFound = true
					} else {
						currentChar = remainder.headOption.getOrElse('_')
						remainder = remainder.drop(1)
						currentFormatString += currentChar
					}
				} // while
			} else {
				output.append(currentChar)
			}

			if (remainder.isEmpty) {
				isDone = true
			}
		}

		output.toString
	}
}
