package scala.c.engine.cFunctions

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import java.io.File
import java.util.{Formatter, Locale}
import scala.c.engine.*
import scala.c.engine.cFunctions.Stdio
import scala.c.engine.models.*
import scala.collection.mutable.ListBuffer
import scala.util.Try

object Functions {

	var varArgStartingAddr = List[Int]()

	val scalaFunctions = ListBuffer[Function]()

	scalaFunctions += new EmulatedFunction("_assert") {
		def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
			val addy = formattedOutputParams(0).value.asInstanceOf[Int]
			println(Utils.readString(addy)(using state) + " FAILED")
			None
		}
	}

	Stdio.addFunctions(scalaFunctions)
	Mathh.addFunctions(scalaFunctions)
	Stdlibh.addFunctions(scalaFunctions)
	Stringh.addFunctions(scalaFunctions)
	Stdargh.addFunctions(scalaFunctions)
}

abstract class RealFunction(name: String) extends Function(name, true)
abstract class EmulatedFunction(name: String) extends Function(name, false)

// 'isNative' implies the function is in C, not Scala
abstract class Function(val name: String, val isNative: Boolean) {
	var index = -1
	var node: IASTNode = _
	val staticVars: List[Variable] = List()

	def run(formattedOutputParams: Array[RValue], state: State): Option[RValue]
}