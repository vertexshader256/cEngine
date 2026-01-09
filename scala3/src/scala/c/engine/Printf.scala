package scala.c.engine

import java.util.{Formatter, Locale}
import scala.collection.mutable.ListBuffer

// function which I use to simulate C's standard printf()
object Printf {

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
						currentFormatString += 'f'
						var buffer2 = new StringBuffer()
						var formatter2 = new Formatter(buffer2, Locale.US)
						val resolved = new ListBuffer[Object]()
						val base = TypeHelper.toRValue(varArgs(paramCount))(using state).value.asInstanceOf[Object]

						val convert = base match
							case boolean: java.lang.Boolean =>
								if boolean then 1.0 else 0.0
							case int: java.lang.Integer =>
								int.toFloat
							case _ =>
								base

						println("FORMAT STR: " + currentFormatString)
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

						paramCount += 1
						remainder = remainder.drop(1)
						wasFormatStringFound = true
					} else if (remainder.startsWith("d")) {
						val buffer2 = new StringBuffer()
						val formatter2 = new Formatter(buffer2, Locale.US)
						val resolved = new ListBuffer[Object]()

						val num = TypeHelper.toRValue(varArgs(paramCount))(using state).value
						currentFormatString += 'd'

						num match
							case long: Long => resolved += Int.box(long.toInt)
							case _ => resolved += convertBoolean()

						formatter2.format("%" + currentFormatString, resolved.toSeq *)
						println("HERE")
						output.append(buffer2.toString)

						paramCount += 1
						remainder = remainder.drop(1)
						wasFormatStringFound = true
					} else if (remainder.startsWith("u")) { // unsigned
						val buffer2 = new StringBuffer()
						val formatter2 = new Formatter(buffer2, Locale.US)
						val resolved = new ListBuffer[Object]()

						val x = TypeHelper.toRValue(varArgs(paramCount))(using state).value

						val value = x match
							case long: Long => long & 0xFFFFFFFFL
							case _ => TypeHelper.castToUnsigned(false, x)

						resolved += value.asInstanceOf[Object]

						formatter2.format("%d", resolved.toSeq *)
						println("HERE")
						output.append(buffer2.toString)

						paramCount += 1
						remainder = remainder.drop(1)
						wasFormatStringFound = true
					} else if (remainder.startsWith("llu")) { // long long unsigned
						val buffer2 = new StringBuffer()
						val formatter2 = new Formatter(buffer2, Locale.US)
						val resolved = new ListBuffer[Object]()

						val bigInt = TypeHelper.toRValue(varArgs(paramCount))(using state).value
						val longVal = Long.box(bigInt.asInstanceOf[Long])

						resolved += java.lang.Long.toUnsignedString(Long.box(longVal))

						formatter2.format("%s", resolved.toSeq *)
						println("HERE3")
						output.append(buffer2.toString)

						paramCount += 1
						remainder = remainder.drop(3)
						wasFormatStringFound = true
					} else if (remainder.startsWith("ld")) {
						val buffer2 = new StringBuffer()
						val formatter2 = new Formatter(buffer2, Locale.US)
						val num = TypeHelper.toRValue(varArgs(paramCount))(using state).value
						val resolved = new ListBuffer[Object]()
						resolved += convertBoolean()

						formatter2.format("%d", resolved.toSeq *)
						output.append(buffer2)

						remainder = remainder.drop(2)
						paramCount += 1
						wasFormatStringFound = true
					} else if (remainder.startsWith("lld")) {
						val buffer2 = new StringBuffer()
						val formatter2 = new Formatter(buffer2, Locale.US)
						val num = TypeHelper.toRValue(varArgs(paramCount))(using state).value
						val resolved = new ListBuffer[Object]()

						resolved += convertBoolean()

						formatter2.format("%d", resolved.toSeq *)
						output.append(buffer2)

						remainder = remainder.drop(3)
						paramCount += 1
						wasFormatStringFound = true
					} else if (remainder.startsWith("s")) {
						val buffer2 = new StringBuffer()
						val formatter2 = new Formatter(buffer2, Locale.US)

						val theVal = varArgs(paramCount).value
						val stringAddr = theVal.asInstanceOf[Int]

						val value = if stringAddr != 0 then
							val str = Utils.readString(stringAddr)(using state)
							str.split(System.lineSeparator()).mkString.asInstanceOf[Object]
						else
							"(null)".asInstanceOf[Object]

						formatter2.format("%s", List(value) *)
						output.append(buffer2)

						remainder = remainder.drop(1)
						paramCount += 1
						wasFormatStringFound = true
					} else if (remainder.startsWith("c")) {
						val buffer2 = new StringBuffer()
						val formatter2 = new Formatter(buffer2, Locale.US)

						val value = TypeHelper.toRValue(varArgs(paramCount))(using state).value.asInstanceOf[Object]

						formatter2.format("%c", List(value) *)
						output.append(buffer2.toString)

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


	def printf2(formattedOutputParams: Array[RValue], state: State): String = {
		val str = Utils.readString(formattedOutputParams.last.value.asInstanceOf[Int])(using state)

		var percentFound = false
		var paramCount = 0

		val resolved = new ListBuffer[Object]()

		val varArgs = formattedOutputParams.reverse.tail.toList

		def convertBoolean(): Object = {
			val x = TypeHelper.toRValue(varArgs(paramCount))(using state).value
			val convertedBool = x match
				case bool: Boolean => if bool then 1 else 0
				case _ => x

			convertedBool.asInstanceOf[Object]
		}

		var resultingFormatString = ""
		var formatFound = ""

		if (str == "%llu\n") {
			formatFound += "%s"

			val bigInt = TypeHelper.toRValue(varArgs(paramCount))(using state).value
			val longVal = Long.box(bigInt.asInstanceOf[Long])

			resolved += java.lang.Long.toUnsignedString(Long.box(longVal))

			resultingFormatString += formatFound

			percentFound = false
			paramCount += 1
		} else {
			str.toCharArray.foreach { c =>
				if (!percentFound && c == '%') {
					resultingFormatString += c
					formatFound = ""
					percentFound = true
				} else if (percentFound && c == 's') {
					percentFound = false
					val theVal = varArgs(paramCount).value
					val stringAddr = theVal.asInstanceOf[Int]

					if stringAddr != 0 then
						val str = Utils.readString(stringAddr)(using state)
						resolved += str.split(System.lineSeparator()).mkString.asInstanceOf[Object]
					else
						resolved += "(null)".asInstanceOf[Object]

					formatFound += c
					resultingFormatString += formatFound
					paramCount += 1
				} else if (percentFound && (c == 'u')) {
					formatFound += 'd'

					val x = TypeHelper.toRValue(varArgs(paramCount))(using state).value

					val value = x match
						case long: Long => long & 0xFFFFFFFFL
						case _ => TypeHelper.castToUnsigned(false, x)

					resolved += value.asInstanceOf[Object]

					resultingFormatString += 'd'
					percentFound = false
					paramCount += 1
				} else if (percentFound && (c == 'd')) {
					formatFound += c

					if formatFound != "ld" && formatFound != "lld" then {
						val num = TypeHelper.toRValue(varArgs(paramCount))(using state).value

						num match
							case long: Long => resolved +=
								// trying to print a long with the %d format
								Int.box(long.toInt)
							case _ => resolved += convertBoolean()

						resultingFormatString += formatFound
					} else
						resolved += convertBoolean()
						resultingFormatString += 'd'

					percentFound = false
					paramCount += 1
				} else if (percentFound && c == 'c') {
					resolved += TypeHelper.toRValue(varArgs(paramCount))(using state).value.asInstanceOf[Object]
					percentFound = false
					formatFound += c
					resultingFormatString += formatFound
					paramCount += 1
				} else if (percentFound && c == 'f') {
					formatFound += c

					val buffer2 = new StringBuffer()
					val formatter2 = new Formatter(buffer2, Locale.US)

					val base = TypeHelper.toRValue(varArgs(paramCount))(using state).value.asInstanceOf[Object]

					val convert = base match
						case boolean: java.lang.Boolean =>
							if boolean then 1.0 else 0.0
						case int: java.lang.Integer =>
							int.toFloat
						case _ =>
							base

					formatter2.format("%f", List(convert) *)

					if (buffer2.toString.contains("Infinity") || buffer2.toString.contains("NaN")) {
						resultingFormatString += 's'
						resolved += buffer2.toString.replace("Infinity", "inf")
							.replace("NaN", "-nan(ind)")
					} else {
						resultingFormatString += formatFound

						base match
							case boolean: java.lang.Boolean =>
								val converted = if boolean then 1.0f else 0.0f
								resolved += Float.box(converted)
							case int: java.lang.Integer =>
								resolved += Float.box(int.toFloat)
							case _ =>
								resolved += base
					}

					percentFound = false
					paramCount += 1
				} else if (percentFound) {
					formatFound += c
				} else {
					resultingFormatString += c
				}
			}
		}

		val output = new StringBuffer()
		val formatter = new Formatter(output, Locale.US)
		formatter.format(resultingFormatString, resolved.toSeq *)

		output.toString
	}
}
