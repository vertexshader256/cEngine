package scala.c.engine

import java.util.{Formatter, Locale}
import scala.collection.mutable.ListBuffer

// function which I use to simulate C's standard printf()
object Printf {
	def printf(formattedOutputParams: Array[RValue], state: State): String = {
		val str = Utils.readString(formattedOutputParams.last.value.asInstanceOf[Int])(using state)

		val buffer = new StringBuffer()
		val formatter = new Formatter(buffer, Locale.US)

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

		println(str)

		if (str == "%llu\n") {
			formatFound += "%s"

			println("HERE: " + varArgs(paramCount) + "|")

			val bigInt = TypeHelper.toRValue(varArgs(paramCount))(using state).value

			println(bigInt)

			val longVal = Long.box(bigInt.asInstanceOf[Long])
			println(":::" + longVal)

			resolved += java.lang.Long.toUnsignedString(Long.box(longVal))

			//println("BLAH2: " + Long.box(java.lang.Long.toUnsignedString().parseUnsignedLong("18446744073709551615")))
			//println("BLAH: " + longVal)

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

		println("---")
		println(resultingFormatString)
		println(resolved)

		formatter.format(resultingFormatString, resolved.toSeq *)

		buffer.toString
	}
}
