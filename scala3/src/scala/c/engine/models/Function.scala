package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.IASTNode

import scala.c.engine.State

// 'isNative' implies the function is in C, not Scala
abstract class Function(val name: String, val isNative: Boolean) {
	var index = -1
	var node: IASTNode = _
	val staticVars: List[Variable] = List()

	def run(formattedOutputParams: Array[RValue], state: State): Option[RValue]
}
