package scala.c.engine.cFunctions

import scala.c.engine.*
import scala.c.engine.models.{Function, *}
import scala.collection.mutable.ListBuffer

object Stdlibh {

	def getFunctions(): List[FunctionDef] = {
		/////////////////////////////////////////////////////////////////
		//                  <stdlib.h> functions                       //
		/////////////////////////////////////////////////////////////////

		val scalaFunctions = new ListBuffer[FunctionDef]

		scalaFunctions += new OneParameterFunction[Address]("free") {
			def func(addr: Address) = {
				None // stub
			}
		}

		scalaFunctions += new ZeroParameterFunction("rand") {
			def func() = {
				Some(Math.abs(scala.util.Random.nextInt()))
			}
		}

		scalaFunctions += new TwoParameterFunction[Int, Int]("calloc") {
			def func(blockSize: Int, numBlocks: Int) = {
				val addr = state.allocateHeapSpace(numBlocks * blockSize)
				state.stack.clearMemory(addr, numBlocks * blockSize)
				Some(addr.location)
			}
		}

		scalaFunctions += new OneParameterFunction[Int]("malloc") {
			def func(numBytes: Int) = {
				Some(state.allocateHeapSpace(numBytes).location)
			}
		}

		scalaFunctions += new OneParameterFunction[Int]("realloc") {
			def func(numBytes: Int) = {
				Some(state.allocateHeapSpace(numBytes).location)
			}
		}

		scalaFunctions += new OneParameterFunction[Address]("atoi") {
			def func(str: Address) = {
				val string = Utils.readString(str)
				Some(string.toInt)
			}
		}

		scalaFunctions.result
	}
}
