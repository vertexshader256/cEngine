package scala.c.engine

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.{CASTFunctionCallExpression, CASTIdExpression, CASTName, CVariable}

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Stack}

case class NodePath(node: IASTNode, var direction: Direction)

class VariableScope() {
  var varMap = new mutable.LinkedHashMap[String, Variable]() // linked to keep deterministic
}

class FunctionScope(val staticVars: List[Variable], val parent: FunctionScope, val returnType: IType) {
  var variableScopes = List[VariableScope](new VariableScope()) // linked to keep deterministic

  private var stack = List[ValueType]()
  var startingStackAddr = 0

  private var pathStack = ListBuffer[Any]()
  private var pathIndex = 0

  var state: State = null

  def pushVariableScope() = {
    variableScopes = new VariableScope() +: variableScopes
  }

  def popVariableScope() = {
    variableScopes = variableScopes.tail
  }

  def resolveId(name: IASTName): Option[Variable] = {
    staticVars.find{_.name == name.getRawSignature}.orElse {
      variableScopes.flatMap { scope =>
        scope.varMap.get(name.getRawSignature)
      }.headOption
        .orElse(if (parent != null) parent.resolveId(name) else None)
        .orElse(Some(state.functionPointers(name.getRawSignature)))
    }
  }

  def addVariable(name: String, theType: IType): Variable = {
    staticVars.find{_.name == name}.getOrElse {
      val newVar = Variable(name, state, theType)
      variableScopes.head.varMap += newVar.name -> newVar
      newVar
    }
  }

  def continue() = {
    while (!pathStack(state.context.pathIndex).isInstanceOf[ContinueLabel]) {
      pathIndex += 1
    }
  }

  def jmpRelative(incrementBy: Int) = {
    pathIndex += incrementBy
  }

  def setAddress(addr: Int) = {
    pathIndex = addr
  }

  def pushOntoStack(values: List[ValueType]) = {
    stack = values.reverse ++: stack
  }

  def popStack: ValueType = {
    val retVal = stack.head
    stack = stack.tail
    retVal
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
      case x => x.printStackTrace()
    }
  }

  def init(node: IASTNode, theState: State, shouldReset: Boolean) {
    if (shouldReset) {
      variableScopes.head.varMap.clear
    }

    stack = List()
    startingStackAddr = theState.Stack.insertIndex

    pathStack ++= State.flattenNode(node)(theState)

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
    val index = pathIndex
    val current = if (index >= pathStack.size) null else pathStack(index)
    if (current != null) {

//      if (current.isInstanceOf[IASTNode]) {
//        println(current.getClass.getSimpleName + ":" + index + ":" + current.asInstanceOf[IASTNode].getRawSignature)
//        println(Utils.getDescendants(current.asInstanceOf[IASTNode]).map(_.getClass.getSimpleName))
//      } else {
//        println(current.getClass.getSimpleName + ":" + index)
//      }

      ast.Ast.step(current)(state)

      pathIndex += 1

      true
    } else {
      false
    }
  }
}