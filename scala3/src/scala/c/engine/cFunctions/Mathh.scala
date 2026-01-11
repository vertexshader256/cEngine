package scala.c.engine.cFunctions

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import java.io.File
import java.util.{Formatter, Locale}
import scala.c.engine.*
import scala.c.engine.models.*
import scala.collection.mutable.ListBuffer
import scala.util.Try

object Mathh {

	def addFunctions(scalaFunctions: ListBuffer[Function]) = {
		/////////////////////////////////////////////////////////////////
		//                   <math.h> functions                        //
		/////////////////////////////////////////////////////////////////

		scalaFunctions += new Function("modf", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val fraction = formattedOutputParams(0).value.asInstanceOf[Double]
				val intPart = formattedOutputParams(1).value.asInstanceOf[Int]

				state.Stack.writeToMemory(fraction.toInt, intPart, TypeHelper.intType)

				Some(RValue(fraction % 1.0))
			}
		}

		scalaFunctions += new Function("sqrt", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val num = formattedOutputParams(0).value.asInstanceOf[Double]
				Some(RValue(Math.sqrt(num)))
			}
		}

		scalaFunctions += new Function("fabs", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.abs(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("sin", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.sin(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("cos", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.cos(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("tan", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.tan(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("acos", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.acos(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("cosh", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.cosh(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("asin", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.asin(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("sinh", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.sinh(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("atan", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.atan(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("tanh", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.tanh(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("atan2", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.atan2(formattedOutputParams.last.value.asInstanceOf[Double],
					formattedOutputParams.head.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("log10f", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.log10(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("exp", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.exp(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("ceil", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.ceil(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("floor", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.floor(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("log", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.log(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("log10", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.log10(formattedOutputParams.last.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("pow", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.pow(formattedOutputParams.last.value.asInstanceOf[Double],
					formattedOutputParams.head.value.asInstanceOf[Double])))
			}
		}

		scalaFunctions += new Function("fmod", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val first = TypeHelper.cast(formattedOutputParams.last.value, TypeHelper.doubleType).value.asInstanceOf[Double]
				val second = TypeHelper.cast(formattedOutputParams.head.value, TypeHelper.doubleType).value.asInstanceOf[Double]

				Some(RValue(first % second))
			}
		}

		scalaFunctions += new Function("sscanf", false) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val resultBuffer = formattedOutputParams.last.value.asInstanceOf[Int]

				val varArgs = formattedOutputParams.drop(2).toList

				val result = Printf.printf(formattedOutputParams, state)

				state.writeDataBlock(result.getBytes, resultBuffer)

				Some(RValue(varArgs.size))
			}
		}

		scalaFunctions += new Function("printf", false) {
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
