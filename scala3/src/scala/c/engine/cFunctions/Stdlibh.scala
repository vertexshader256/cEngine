package scala.c.engine.cFunctions

import scala.c.engine.*
import scala.c.engine.models.{Function, *}
import scala.collection.mutable.ListBuffer

object Stdlibh {

	/////////////////////////////////////////////////////////////////
	//                  <stdlib.h> functions                       //
	/////////////////////////////////////////////////////////////////

	def addFunctions(scalaFunctions: ListBuffer[Function])(implicit theState: CEngine) = {

		scalaFunctions += new OneParameterFunction[Address]("free") {
			def func(addr: Address) = {
				None // stub
			}
		}.generate

		scalaFunctions += new ZeroParameterFunction("rand") {
			def func() = {
				Some(Math.abs(scala.util.Random.nextInt()))
			}
		}.generate

		scalaFunctions += new TwoParameterFunction[Int, Int]("calloc") {
			def func(blockSize: Int, numBlocks: Int) = {
				val addr = state.allocateHeapSpace(numBlocks * blockSize)
				state.memory.clearMemory(addr, numBlocks * blockSize)
				Some(addr.location)
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Int]("malloc") {
			def func(numBytes: Int) = {
				Some(state.allocateHeapSpace(numBytes).location)
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Int]("realloc") {
			def func(numBytes: Int) = {
				Some(state.allocateHeapSpace(numBytes).location)
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
