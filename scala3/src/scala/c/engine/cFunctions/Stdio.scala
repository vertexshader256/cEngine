package scala.c.engine.cFunctions

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import java.io.File
import java.util.{Formatter, Locale}
import scala.c.engine.*
import scala.c.engine.models.{Function, *}
import scala.collection.mutable.ListBuffer
import scala.util.Try

object Stdio {

	/////////////////////////////////////////////////////////////////
	//                   <stdio.h> functions                       //
	/////////////////////////////////////////////////////////////////

	def addFunctions(scalaFunctions: ListBuffer[Function])(implicit theState: CEngine) = {

		scalaFunctions += new EmulatedFunction("printf") {
			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				state.stdout ++= Printf.printf(formattedOutputParams, state)
				None
			}
		}

		scalaFunctions += new EmulatedFunction("sscanf") {
			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				val resultBuffer = formattedOutputParams.last.value.asInstanceOf[Int]

				val varArgs = formattedOutputParams.drop(2).toList

				val result = Printf.printf(formattedOutputParams, state)

				state.writeDataBlock(Address(resultBuffer), result.getBytes)

				Some(RValue(varArgs.size))
			}
		}

		//fcvtbuf(double arg, int ndigits, int *decpt, int *sign, char *buf)
		scalaFunctions += new EmulatedFunction("fcvtbuf") {
			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				//val buf = formattedOutputParams(0).value.asInstanceOf[Int]
				//val sign = formattedOutputParams(1).value.asInstanceOf[Int]
				val decpt = formattedOutputParams(2).value.asInstanceOf[Int]
				val ndigits = formattedOutputParams(3).value.asInstanceOf[Int]
				val arg = formattedOutputParams(4).value.asInstanceOf[Double]

				state.memory.writeToMemory(1, Address(decpt), TypeHelper.intType)

				val buffer = StringBuffer()
				val formatter = Formatter(buffer, Locale.US)

				val formatString = "%." + ndigits + "f"

				val args = Array[Object](arg.asInstanceOf[Object])

				formatter.format(formatString, args *)

				val result1 = buffer.toString
				val index = result1.indexOf('.')
				val resultString = result1.replace(".", "")

				val array = resultString.toCharArray.map { char => RValue(char.toByte, TypeHelper.charType) }.toList

				state.memory.writeToMemory(index, Address(decpt), TypeHelper.intType)

				// to-do: find a way to do this without allocating?
				val result = state.allocateHeapSpace(20)

				state.writeValues(result, array)
				Some(RValue(result.location))
			}
		}

		scalaFunctions += new OneParameterFunction[Char]("putchar") {
			def func(char: Char) = {
				state.stdout += char
				None
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Address]("puts") {
			def func(str: Address) = {
				val string = Utils.readString(str)
				val tabsReplaced = string.replace("\\t", "\t")

				tabsReplaced.foreach: char =>
					state.stdout += char

				state.stdout += '\n'
				None
			}
		}.generate

		scalaFunctions += new EmulatedFunction("fopen") {
			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				val path = Utils.readString(Address(formattedOutputParams.last.value.asInstanceOf[Int]))(using state)
				val mode = Utils.readString(Address(formattedOutputParams.head.value.asInstanceOf[Int]))(using state)

				if (!File(path).exists()) {
					if (mode == "w") {
						File(path).createNewFile()
						Some(FileRValue(path))
					} else {
						Some(FileRValue(path))
					}
				} else {
					Some(FileRValue(path))
				}
			}
		}

		// a return value of 0 indicates the file was successfully deleted
		scalaFunctions += new EmulatedFunction("remove") {
			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				val path = Utils.readString(Address(formattedOutputParams.last.value.asInstanceOf[Int]))(using state)
				val file = File(path)

				Try(file.delete()).toOption.map { wasDeleted =>
					if wasDeleted then
						RValue(0)
					else
						RValue(-1)
				}.orElse(Some(RValue(-1)))
			}
		}

		scalaFunctions += new EmulatedFunction("fgets") {
			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				val resultBuffer = formattedOutputParams(2).value.asInstanceOf[Int]
				val size = formattedOutputParams(1).value.asInstanceOf[Int]
				val fp = formattedOutputParams(0).asInstanceOf[FileRValue]

				val result = new ListBuffer[Byte]()
				var count = 0
				var lastRead: Byte = 0
				var isDone = false

				while (count < size && lastRead.toChar != '\n' && !isDone) {
					val z = fp.read(1)
					if (z.isEmpty) {
						isDone = true
					} else {
						lastRead = z.head
						result += lastRead
						count += 1
					}
				}

				state.writeDataBlock(Address(resultBuffer), result.toArray)

				None
			}
		}

		scalaFunctions += new EmulatedFunction("getc") {
			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				val fp = formattedOutputParams(0).asInstanceOf[FileRValue]

				try {
					val character: cEngVal = java.lang.Byte.toUnsignedInt(fp.read(1).head).toByte

					Some(RValue(character, TypeHelper.charType))
				} catch {
					case e: Throwable => Some(RValue(-1))
				}
			}
		}

		// returns 0 on success
		scalaFunctions += new EmulatedFunction("fclose") {
			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				val fp = formattedOutputParams.last.asInstanceOf[FileRValue]
				fp.close()
				Some(RValue(0))
			}
		}

		scalaFunctions += new EmulatedFunction("fprintf") {
			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				val fp = formattedOutputParams.last.asInstanceOf[FileRValue]

				val formattedStr = Printf.printf(formattedOutputParams.drop(1), state)
				fp.printf(formattedStr)
				None
			}
		}

		scalaFunctions += new EmulatedFunction("snprintf") {
			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				val strAddr = formattedOutputParams.last.value.asInstanceOf[Int]
				val size = formattedOutputParams.reverse(1).value.asInstanceOf[Int]

				val formattedStr = Printf.printf(formattedOutputParams.reverse.drop(2).reverse, state)
				state.writeDataBlock(Address(strAddr), formattedStr.getBytes)

				val resultSize = formattedStr.length

				Some(RValue(resultSize))
			}
		}

		scalaFunctions += new EmulatedFunction("sprintf") {
			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				val strAddr = formattedOutputParams.last.value.asInstanceOf[Int]

				val formattedStr = Printf.printf(formattedOutputParams.drop(1), state)
				state.writeDataBlock(Address(strAddr), formattedStr.getBytes)
				None
			}
		}

		// TODO: Complete this
		scalaFunctions += new EmulatedFunction("fscanf") {
			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				//val fp = formattedOutputParams(3).asInstanceOf[FileRValue]
				//val dst = formattedOutputParams(1).value.asInstanceOf[Int]

				//val str = fp.readString
				//state.writeDataBlock(str.getBytes, dst)(state)
				None
			}
		}

		scalaFunctions += new EmulatedFunction("fread") {
			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				val resultBuffer = formattedOutputParams(3).value.asInstanceOf[Int]
				val size = formattedOutputParams(2).value.asInstanceOf[Int]
				val numMembers = TypeHelper.cast(formattedOutputParams(1).value, TypeHelper.intType).value.asInstanceOf[Int]
				val fp = formattedOutputParams(0).asInstanceOf[FileRValue]

				state.writeDataBlock(Address(resultBuffer), fp.read(numMembers * size))
				Some(RValue(numMembers))
			}
		}

		scalaFunctions += new EmulatedFunction("fwrite") {
			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				val buffer = formattedOutputParams(3).value.asInstanceOf[Int] // write this to fp
				val size = formattedOutputParams(2).value.asInstanceOf[Int]
				val numMembers = formattedOutputParams(1).value.asInstanceOf[Int]
				val fp = formattedOutputParams(0).asInstanceOf[FileRValue]

				val bytes = state.readDataBlock(Address(buffer), size * numMembers)

				fp.write(bytes)

				None
			}
		}
	}
}
