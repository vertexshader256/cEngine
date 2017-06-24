package scala.c.engine

import c.engine._
import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Stack}

case class NodePath(node: IASTNode, var direction: Direction)

class FunctionScope(theStaticVars: List[Variable], node: IASTNode, parent: Scope, val function: IFunctionType, theState: State)
  extends Scope(theStaticVars, node, parent, theState) {
  val labels = new ListBuffer[(String, Stack[NodePath])]()

  val isBreakable = false
  val isContinuable = false
}

class LoopScope(theStaticVars: List[Variable], node: IASTNode, parent: Scope, theState: State)
  extends Scope(theStaticVars, node, parent, theState) {

  val isBreakable = true
  val isContinuable = true
}

abstract class Scope(staticVars: List[Variable], val node: IASTNode, parent: Scope, state: State) {
  private var varMap = new mutable.HashSet[Variable]()
  val pathStack = new Stack[NodePath]()
  val stack = new Stack[ValueType]()
  var startingStackAddr = 0

  reset

  val isBreakable: Boolean
  val isContinuable: Boolean

  def resolveId(name: IASTName): Option[Variable] = {
    varMap.find{_.name.getRawSignature == name.getRawSignature}
      .orElse(if (parent != null) parent.resolveId(name) else None)
      .orElse(Some(state.functionPointers(name.getRawSignature)))
  }

  def reset = {
    varMap.clear
    varMap ++= staticVars.toSet.toList
    pathStack.clear
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