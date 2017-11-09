package scala.c.engine

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.{CASTFunctionCallExpression, CASTIdExpression, CASTName}

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Stack}

case class NodePath(node: IASTNode, var direction: Direction)

class FunctionScope(theStaticVars: List[Variable], node: IASTNode, parent: Scope, val function: IFunctionType, theState: State)
  extends Scope(theStaticVars, node, parent, theState) {

  pathStack = State.flattenNode(node)(theState)

  var instructionCounter = 0
  pathStack.foreach{ node =>
    if (node.isInstanceOf[Label]) {
      node.asInstanceOf[Label].address = instructionCounter
    }
    instructionCounter += 1
  }

  pathStack.collect{case jmpName: JmpName => jmpName}.foreach{ node =>
    pathStack.find{label => label.isInstanceOf[GotoLabel] &&
      label.asInstanceOf[GotoLabel].name == node.label}.foreach { labelFound =>
      node.destAddress = labelFound.asInstanceOf[GotoLabel].address
    }
  }
}

abstract class Scope(staticVars: List[Variable], val node: IASTNode, val parent: Scope, state: State) {
  private var varMap = new mutable.HashSet[Variable]()

  val stack = new Stack[ValueType]()
  var startingStackAddr = 0

  var pathStack = List[Any]()
  var pathIndex = 0

  reset

  def run = {

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
    varMap.find{_.name.getRawSignature == name.getRawSignature}
      .orElse(if (parent != null) parent.resolveId(name) else None)
      .orElse(Some(state.functionPointers(name.getRawSignature)))
  }

  def reset = {
    varMap.clear
    varMap ++= staticVars.toSet.toList
    //pathStack.clear
    stack.clear
    startingStackAddr = state.Stack.insertIndex
  }

  def addArrayVariable(name: IASTName, theType: IArrayType, dimensions: Seq[Int]): ArrayVariable = {
    staticVars.find{_.name.getRawSignature == name.getRawSignature}.getOrElse {
      val newVar = new ArrayVariable(name, state, theType, dimensions)
      varMap.remove(newVar)
      varMap += newVar
      newVar
    }.asInstanceOf[ArrayVariable]
  }

  def addVariable(name: IASTName, theType: IType): Variable = {
    staticVars.find{_.name.getRawSignature == name.getRawSignature}.getOrElse {
      val newVar = new Variable(name, state, theType)
      varMap.remove(newVar)
      varMap += newVar
      newVar
    }
  }
}