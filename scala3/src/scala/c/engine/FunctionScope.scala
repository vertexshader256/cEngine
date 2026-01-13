package scala
package c
package engine

import org.eclipse.cdt.core.dom.ast.*

import scala.c.engine.Instructions.*
import scala.c.engine.models.*
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class VariableScope(val parent: VariableScope) {
	var varMap = mutable.LinkedHashMap[String, Variable]() // linked to keep deterministic

	def resolveId(name: IASTName): Option[Variable] = {
		varMap.get(name.toString).orElse {
			if parent != null then
				parent.resolveId(name)
			else
				None
		}
	}
}

class FunctionScope(val staticVars: List[Variable], val parent: FunctionScope, val returnType: IType) {
	private var currentVariableScope = VariableScope(null)

	private val stack = mutable.Stack[ValueType]()
	var startingStackAddr = 0

	private val pathStack = ListBuffer[IASTNode | CEngineInstruction]()
	private var pathIndex = 0

	var state: State = _

	def pushVariableScope(): Unit = {
		val newScope = VariableScope(currentVariableScope)
		currentVariableScope = newScope
	}

	def popVariableScope(): Unit = {
		if currentVariableScope.parent != null then
			currentVariableScope = currentVariableScope.parent
	}

	def resolveId(name: IASTName): Option[Variable] = {
		staticVars.find {
			_.name == name.toString
		}.orElse {
			currentVariableScope.resolveId(name)
				.orElse(if (parent != null) parent.resolveId(name) else None)
				.orElse(Some(state.functionPointers(name.toString)))
		}
	}

	def addVariable(variable: Variable): Unit = {
		currentVariableScope.varMap += variable.name -> variable
	}

	def addVariable(name: String, theType: IType): Variable = {
		staticVars.find {
			_.name == name
		}.getOrElse {
			val newVar = Variable(name, state, theType)
			currentVariableScope.varMap += newVar.name -> newVar
			newVar
		}
	}

	def addExternVariable(name: String, theType: IType): Variable = {

		var result: Variable = null

		if (parent == null) { // this extern is not in a function
			result = addVariable(name, theType)
		} else {
			if (parent.currentVariableScope.varMap.contains(name) && result == null) {
				currentVariableScope.varMap += name -> parent.currentVariableScope.varMap(name)
				result = parent.currentVariableScope.varMap(name)
			}
		}

		result
	}

	def addArrayVariable(name: String, theType: IType, initVals: List[RValue]): Variable = {
		staticVars.find {
			_.name == name
		}.getOrElse {
			val newVar = Variable(name, state, theType, initVals)
			currentVariableScope.varMap += newVar.name -> newVar
			newVar
		}
	}

	def jmpRelative(incrementBy: Int): Unit = {
		pathIndex += incrementBy
	}

	def setAddress(addr: Int): Unit = {
		pathIndex = addr
	}

	def pushOntoStack(values: List[ValueType]): Unit = {
		stack.pushAll(values.reverse)
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

	def run(theState: State): Unit = {
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

	def init(nodes: List[IASTNode], theState: State, shouldReset: Boolean): Unit = {
		if (shouldReset) {
			currentVariableScope.varMap.clear()
		}

		stack.clear()
		startingStackAddr = theState.Stack.insertIndex

		nodes.foreach { node =>
			pathStack ++= Compiler.compile(node)(using theState)
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

	private def tick(state: State): Boolean = {
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