package scala.c.engine.cFunctions

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import java.io.File
import java.util.{Formatter, Locale}
import scala.c.engine.*
import scala.c.engine.models.{Function, *}
import scala.collection.mutable.ListBuffer
import scala.util.Try

object Mathh {

	def addFunctions(scalaFunctions: ListBuffer[Function]) = {
		/////////////////////////////////////////////////////////////////
		//                   <math.h> functions                        //
		/////////////////////////////////////////////////////////////////

		scalaFunctions += new EmulatedFunction("modf") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val fraction = formattedOutputParams(0).value.asInstanceOf[Double]
				val intPart = formattedOutputParams(1).value.asInstanceOf[Int]

				state.stack.writeToMemory(fraction.toInt, intPart, TypeHelper.intType)

				Some(RValue(fraction % 1.0))
			}
		}

		scalaFunctions += new EmulatedFunction("sqrt") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val num = formattedOutputParams(0).value match {
					case float: Float => float
					case double: Double => double.toFloat
				}

				Some(RValue(Math.sqrt(num)))
			}
		}

		scalaFunctions += new EmulatedFunction("fabs") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.abs(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("sin") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.sin(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("cos") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.cos(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("tan") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.tan(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("acos") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.acos(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("cosh") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.cosh(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("asin") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.asin(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("sinh") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.sinh(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("atan") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.atan(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("tanh") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.tanh(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("atan2") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.atan2(formattedOutputParams.last.value.asInstanceOf[Double],
					formattedOutputParams.head.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("log10f") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.log10(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("exp") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.exp(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("ceil") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.ceil(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("floor") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.floor(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("log") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.log(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("log10") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.log10(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("pow") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.pow(formattedOutputParams.last.value.asInstanceOf[Double],
					formattedOutputParams.head.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new EmulatedFunction("fmod") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val first = TypeHelper.cast(formattedOutputParams.last.value, TypeHelper.doubleType).value.asInstanceOf[Double]
				val second = TypeHelper.cast(formattedOutputParams.head.value, TypeHelper.doubleType).value.asInstanceOf[Double]

				Some(RValue(first % second))
			}
		}

		scalaFunctions += new EmulatedFunction("sscanf") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val resultBuffer = formattedOutputParams.last.value.asInstanceOf[Int]

				val varArgs = formattedOutputParams.drop(2).toList

				val result = Printf.printf(formattedOutputParams, state)

				state.writeDataBlock(Address(resultBuffer, state.stack), result.getBytes)

				Some(RValue(varArgs.size))
			}
		}

		scalaFunctions += new EmulatedFunction("printf") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {

				val result = Printf.printf(formattedOutputParams, state)

				result.getBytes.foreach { char =>
					state.callFunctionFromScala("putchar", Array(RValue(char.toInt, TypeHelper.intType)))
				}

				None
			}
		}
	}
	
}
