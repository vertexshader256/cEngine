package scala.c.engine

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.{CASTFunctionCallExpression, CASTIdExpression, CASTName, CVariable}

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Stack}

case class NodePath(node: IASTNode, var direction: Direction)

class FunctionScope(theStaticVars: List[Variable], parent: Scope, val function: IFunctionType)
  extends Scope(theStaticVars, parent) {

  def init(node: IASTNode, theState: State, shouldReset: Boolean) {
    if (shouldReset) {
      reset(theState)
    }

    pathStack ++= State.flattenNode(node)(theState)

    var instructionCounter = 0
    pathStack.foreach { node =>
      if (node.isInstanceOf[Label]) {
        node.asInstanceOf[Label].address = instructionCounter
      }
      instructionCounter += 1
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
}

abstract class Scope(staticVars: List[Variable], val parent: Scope) {
  var varMap = new mutable.LinkedHashSet[Variable]() // linked to keep deterministic

  val stack = new Stack[ValueType]()
  var startingStackAddr = 0

  var pathStack = ListBuffer[Any]()
  var pathIndex = 0

  var state: State = null

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

  def resolveId(name: IASTName): Option[Variable] = {
    varMap.find{_.name == name.getRawSignature}
      .orElse(if (parent != null) parent.resolveId(name) else None)
      .orElse(Some(state.functionPointers(name.getRawSignature)))
  }

  def reset(state: State) = {
    varMap.clear
    varMap ++= staticVars.toSet.toList
    //pathStack.clear
    stack.clear
    startingStackAddr = state.Stack.insertIndex
  }

  def addVariable(name: String, theType: IType): Variable = {
    staticVars.find{_.name == name}.getOrElse {
      val newVar = Variable(name, state, theType)
      varMap.remove(newVar)
      varMap += newVar
      newVar
    }
  }
}