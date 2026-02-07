package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.IASTNode

import scala.c.engine.State
import scala.collection.mutable.ListBuffer

// 'isNative' implies the function is in C, not Scala
abstract class Function(val name: String, val isNative: Boolean) {
	var index = -1
	var node: IASTNode = _
	private val staticVars = new ListBuffer[Variable]()

	def getStaticVariable(name: String): Option[Variable] = {
		staticVars.find(_.name == name)
	}

	def addStaticVariable(variable: Variable): Unit = {
		staticVars += variable
	}

	def run(formattedOutputParams: Array[RValue], state: State): Option[RValue]
}

abstract class EmulatedFunction(name: String) extends Function(name, false)
