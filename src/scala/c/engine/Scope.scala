package c.engine

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

class SwitchScope(theStaticVars: List[Variable], node: IASTNode, parent: Scope, theState: State)
  extends Scope(theStaticVars, node, parent, theState) {

  var isCaseFound = false

  val isBreakable = true
  val isContinuable = false
}

abstract class Scope(staticVars: List[Variable], val node: IASTNode, val parent: Scope, state: State) {
  private var varMap = new mutable.HashSet[Variable]()
  val pathStack = new Stack[NodePath]()
  val stack = new Stack[ValueType]()
  var startingStackAddr = 0

  reset

  val isBreakable: Boolean
  val isContinuable: Boolean

  def run(node: IASTNode, state: State) = {

    state.context.pathStack.push(NodePath(node, Stage1))

    var keepRunning = true
    while (keepRunning) {
      try {
        keepRunning = tick(state)
      } catch {
        case e =>
          throw e
      }
    }
  }

  def tick(state: State): Boolean = {
    val current = state.context.pathStack.headOption.getOrElse(null)
    if (current != null) {

      //println(current.node.getClass.getSimpleName + ":" + current.direction)

      val paths: Seq[NodePath] = if (state.isGotoing && current.direction != Stage1) {
        val result = (ast.Ast.step(current, Gotoing)(state) orElse ast.Ast.NoMatch)(Gotoing).map{ x => NodePath(x, Stage1)}

        if (state.context.pathStack.size > 1) {
          state.context.pathStack.pop
        }

        result
      } else {

        val result = (ast.Ast.step(current, current.direction)(state) orElse ast.Ast.NoMatch)(current.direction).map{x => NodePath(x, Stage1)}

        current.direction match {
          case Stage1 => current.direction = Stage2
          case Stage2 => current.direction = Stage3
          case Stage3 => current.direction = PreLoop
          case PreLoop => current.direction = Exiting
          case Exiting => state.context.pathStack.pop
        }

        result
      }

      state.context.pathStack.pushAll(paths.reverse)

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