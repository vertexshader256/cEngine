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

object Stdlibh {

	def addFunctions(scalaFunctions: ListBuffer[Function]) = {
		/////////////////////////////////////////////////////////////////
		//                  <stdlib.h> functions                       //
		/////////////////////////////////////////////////////////////////

		scalaFunctions += new EmulatedFunction("free") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				None
			}
		}

		scalaFunctions += new EmulatedFunction("rand") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(Math.abs(scala.util.Random.nextInt())))
			}
		}

		scalaFunctions += new EmulatedFunction("calloc") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val numBlocks = formattedOutputParams(0).value.asInstanceOf[Int]
				val blockSize = formattedOutputParams(1).value.asInstanceOf[Int]

				val addr = state.allocateHeapSpace(numBlocks * blockSize)

				state.stack.clearMemory(addr, numBlocks * blockSize)

				Some(RValue(addr))
			}
		}

		scalaFunctions += new EmulatedFunction("malloc") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val returnVal = formattedOutputParams.head.value match {
					case long: Long => state.allocateHeapSpace(long.toInt)
					case int: Int => state.allocateHeapSpace(int)
				}
				Some(RValue(returnVal))
			}
		}

		scalaFunctions += new EmulatedFunction("realloc") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				Some(RValue(state.allocateHeapSpace(formattedOutputParams.head.value.asInstanceOf[Long].toInt)))
			}
		}

		scalaFunctions += new EmulatedFunction("atoi") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val str = Utils.readString(formattedOutputParams.last.value.asInstanceOf[Int])(using state)
				Some(RValue(str.toInt))
			}
		}
	}
}
