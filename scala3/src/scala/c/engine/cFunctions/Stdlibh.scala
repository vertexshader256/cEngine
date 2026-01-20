package scala.c.engine.cFunctions

import scala.c.engine.*
import scala.c.engine.models.{Function, *}
import scala.collection.mutable.ListBuffer

object Stdlibh {

	def addFunctions(scalaFunctions: ListBuffer[Function])(implicit theState: State) = {
		/////////////////////////////////////////////////////////////////
		//                  <stdlib.h> functions                       //
		/////////////////////////////////////////////////////////////////

		scalaFunctions += new EmulatedFunction("free") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				None
			}
		}

		scalaFunctions += new ZeroParameterFunction("rand") {
			def func() = {
				Some(Math.abs(scala.util.Random.nextInt()))
			}
		}.generate

		scalaFunctions += new TwoParameterFunction[Int, Int]("calloc") {
			def func(blockSize: Int, numBlocks: Int) = {
				val addr = state.allocateHeapSpace(numBlocks * blockSize)
				state.stack.clearMemory(addr, numBlocks * blockSize)
				Some(addr)
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Int]("malloc") {
			def func(numBytes: Int) = {
				Some(state.allocateHeapSpace(numBytes))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Int]("realloc") {
			def func(numBytes: Int) = {
				Some(state.allocateHeapSpace(numBytes))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Address]("atoi") {
			def func(str: Address) = {
				val string = Utils.readString(str)
				Some(string.toInt)
			}
		}.generate
	}
}
