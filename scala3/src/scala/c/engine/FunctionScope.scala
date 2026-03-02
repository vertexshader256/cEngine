package scala
package c
package engine

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.CASTName

import scala.c.engine.instructions.*
import scala.c.engine.models.{Function, *}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized

class VariableScope(val parent: VariableScope) {
	private val varMap = mutable.LinkedHashMap[String, Variable]() // linked to keep deterministic

	def addVariable(variable: Variable) = {
		varMap += variable.name -> variable
	}

	def clear() = {
		varMap.clear()
	}

	def resolveId(name: String): Option[Variable] = {
		varMap.get(name).orElse {
			if parent != null then
				parent.resolveId(name)
			else
				None
		}
	}
}

class FunctionScope(val function: Function, val parent: FunctionScope, val returnType: IType) {
	private var currentVariableScope = VariableScope(null)

	private val stack = mutable.Stack[ValueType]()
	var startingStackAddr = 0

	private val pathStack = ListBuffer[IASTNode | CEngineInstruction]()
	private var pathIndex = 0

	var state: CEngine = uninitialized

	def pushVariableScope(): Unit = {
		val newScope = VariableScope(currentVariableScope)
		currentVariableScope = newScope
	}

	def popVariableScope(): Unit = {
		if currentVariableScope.parent != null then
			currentVariableScope = currentVariableScope.parent
	}

	def resolveId(name: IASTName): Option[Variable] = {
		function.getStaticVariable(name.toString).orElse {
			currentVariableScope.resolveId(name.toString)
				.orElse(if (parent != null) parent.resolveId(name) else None)
				.orElse(Some(state.functionPointers(name.toString)))
		}
	}

	def addVariable(variable: Variable): Unit = {
		if variable.isStatic then
			function.addStaticVariable(variable)
		else
			currentVariableScope.addVariable(variable)
	}

	// special case when we cant get a IASTName
	def addVariable(name: String, theType: IType): Variable = {
		val iastName = CASTName(name.toCharArray)
		addVariable(iastName, theType)
	}

	def isStaticAlreadyDefined(name: IASTName): Boolean = {
		function.getStaticVariable(name.toString).isDefined
	}

	def addVariable(name: IASTName, theType: IType, initVals: List[RValue] = List()): Variable = {
		function.getStaticVariable(name.toString).getOrElse {
			val newVar = Variable(name, state, theType, initVals)
			addVariable(newVar)
			newVar
		}
	}

	def jmpRelative(incrementBy: Int): Unit = {
		pathIndex += incrementBy
	}

	def setAddress(addr: Int): Unit = {
		pathIndex = addr
	}

	def pushOntoStack(value: ValueType): Unit = {
		stack.push(value)
	}

	def popStack: ValueType = {
		stack.pop()
	}

	def getReturnValue: Option[ValueType] = {
		stack.headOption
	}

	def run(theState: CEngine): Unit = {
		state = theState
		var keepRunning = true
		try {
			while (keepRunning) {
				keepRunning = tick(state)
			}
		} catch {
			case ReturnFromFunction() =>
		}
	}

	def init(nodes: List[IASTNode], theState: CEngine, shouldReset: Boolean): Unit = {
		if (shouldReset) {
			currentVariableScope.clear()
		}

		stack.clear()
		startingStackAddr = theState.memory.getStackPosition

		nodes.foreach { node =>
			pathStack ++= theState.compile(node)(using theState)
		}

		pathStack.zipWithIndex.foreach { case (node, index) =>
			node match
				case label: Label => label.address = index
				case _ =>
		}

		pathStack.collect { case goto: Goto => goto }.foreach { node =>
			pathStack.find { label =>
				label.isInstanceOf[GotoLabel] &&
					label.asInstanceOf[GotoLabel].name == node.label
			}.foreach { labelFound =>
				node.destAddress = labelFound.asInstanceOf[GotoLabel].address
			}
		}
	}

	private def tick(state: CEngine): Boolean = {
		if (pathIndex < pathStack.size) {

			//      if (current.isInstanceOf[IASTNode]) {
			//        println(current.getClass.getSimpleName + ":" + current.asInstanceOf[IASTNode].getRawSignature)
			//        println(Utils.getDescendants(current.asInstanceOf[IASTNode]).map(_.getClass.getSimpleName))
			//      } else {
			//        println(current.getClass.getSimpleName)
			//      }

			ast.Ast.step(pathStack(pathIndex))(using state)
			pathIndex += 1

			true
		} else {
			false
		}
	}
}