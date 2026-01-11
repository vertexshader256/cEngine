package scala
package c
package engine

import org.eclipse.cdt.core.dom.ast.*
import scala.c.engine.models.*
import scala.c.engine.Instructions.*
import scala.c.engine.models.Variable
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class VariableScope() {
	var varMap = mutable.LinkedHashMap[String, Variable]() // linked to keep deterministic
}

class FunctionScope(val staticVars: List[Variable], val parent: FunctionScope, val returnType: IType) {
	val variableScopes = mutable.Stack[VariableScope](VariableScope())

	private val stack = mutable.Stack[ValueType]()
	var startingStackAddr = 0

	private val pathStack = ListBuffer[Any]()
	private var pathIndex = 0

	var state: State = null

	def pushVariableScope() = {
		variableScopes.push(VariableScope())
	}

	def popVariableScope() = {
		variableScopes.pop()
	}

	def resolveId(name: IASTName): Option[Variable] = {
		staticVars.find {
			_.name == name.toString
		}.orElse {
			variableScopes.flatMap { scope =>
					scope.varMap.get(name.toString)
				}.headOption
				.orElse(if (parent != null) parent.resolveId(name) else None)
				.orElse(Some(state.functionPointers(name.toString)))
		}
	}

	def addVariable(variable: Variable): Unit = {
		variableScopes.head.varMap += variable.name -> variable
	}

	def addVariable(name: String, theType: IType): Variable = {
		staticVars.find {
			_.name == name
		}.getOrElse {
			val newVar = Variable(name, state, theType)
			variableScopes.head.varMap += newVar.name -> newVar
			newVar
		}
	}

	def addExternVariable(name: String, theType: IType): Variable = {

		var result: Variable = null

		if (parent == null) { // this extern is not in a function
			result = addVariable(name, theType)
		} else {
			parent.variableScopes.map { scope =>
				if (scope.varMap.contains(name) && result == null) {
					variableScopes.head.varMap += name -> scope.varMap(name)
					result = scope.varMap(name)
				}
			}
		}

		result
	}

	def addArrayVariable(name: String, theType: IType, initVals: List[RValue]): Variable = {
		staticVars.find {
			_.name == name
		}.getOrElse {
			val newVar = Variable(name, state, theType, initVals)
			variableScopes.head.varMap += newVar.name -> newVar
			newVar
		}
	}

	def jmpRelative(incrementBy: Int) = {
		pathIndex += incrementBy
	}

	def setAddress(addr: Int) = {
		pathIndex = addr
	}

	def pushOntoStack(values: List[ValueType]) = {
		stack.pushAll(values.reverse)
	}

	def pushOntoStack(value: ValueType) = {
		stack.push(value)
	}

	def popStack: ValueType = {
		stack.pop()
	}

	def getReturnValue: Option[ValueType] = {
		stack.headOption
	}

	def run(theState: State) = {
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

	def init(nodes: List[IASTNode], theState: State, shouldReset: Boolean) = {
		if (shouldReset) {
			variableScopes.head.varMap.clear()
		}

		stack.clear()
		startingStackAddr = theState.Stack.insertIndex

		nodes.foreach { node =>
			pathStack ++= State.flattenNode(node)(using theState)
		}

		pathStack.zipWithIndex.foreach { case (node, index) =>
			if (node.isInstanceOf[Label]) {
				node.asInstanceOf[Label].address = index
			}
		}

		pathStack.collect { case jmpName: JmpName => jmpName }.foreach { node =>
			pathStack.find { label =>
				label.isInstanceOf[GotoLabel] &&
					label.asInstanceOf[GotoLabel].name == node.label
			}.foreach { labelFound =>
				node.destAddress = labelFound.asInstanceOf[GotoLabel].address
			}
		}
	}

	def tick(state: State): Boolean = {
		val current = if (pathIndex >= pathStack.size) null else pathStack(pathIndex)
		if (current != null) {

			//      if (current.isInstanceOf[IASTNode]) {
			//        println(current.getClass.getSimpleName + ":" + current.asInstanceOf[IASTNode].getRawSignature)
			//        println(Utils.getDescendants(current.asInstanceOf[IASTNode]).map(_.getClass.getSimpleName))
			//      } else {
			//        println(current.getClass.getSimpleName)
			//      }

			ast.Ast.step(current)(using state)

			pathIndex += 1

			true
		} else {
			false
		}
	}
}