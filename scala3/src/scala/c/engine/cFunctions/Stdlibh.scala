package scala.c.engine.cFunctions

import scala.c.engine.*
import scala.c.engine.models.{Function, *}
import scala.collection.mutable.ListBuffer

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
				val ptr = Pointer(Address(addr, state.stack), TypeHelper.void)

				Some(ptr)
			}
		}

		scalaFunctions += new EmulatedFunction("malloc") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val returnVal = formattedOutputParams.head.value match {
					case long: Long => state.allocateHeapSpace(long.toInt)
					case int: Int => state.allocateHeapSpace(int)
				}

				val ptr = Pointer(Address(returnVal, state.stack), TypeHelper.void)
				Some(ptr)
			}
		}

		scalaFunctions += new EmulatedFunction("realloc") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val addr = state.allocateHeapSpace(formattedOutputParams.head.value.asInstanceOf[Long].toInt)
				val ptr = Pointer(Address(addr, state.stack), TypeHelper.void)
				Some(ptr)
			}
		}

		scalaFunctions += new EmulatedFunction("atoi") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val str = Utils.readString(Address(formattedOutputParams.last.value.asInstanceOf[Int], state.stack))(using state)
				Some(RValue(str.toInt))
			}
		}
	}
}
