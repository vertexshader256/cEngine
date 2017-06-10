package scala.c.engine

import c.engine._
import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}

case class NodePath(node: IASTNode, var direction: Direction)

class FunctionScope(theStaticVars: List[Variable], parent: Scope, val function: IFunctionType, theState: State)
  extends Scope(theStaticVars, parent, theState) {
  val labels = new ListBuffer[(String, Stack[NodePath])]()
}

trait ContinuableScope
trait BreakableScope

class LoopScope(theStaticVars: List[Variable], parent: Scope, theState: State)
  extends Scope(theStaticVars, parent, theState) with ContinuableScope with BreakableScope {
}

object Scope {
  val varMap = scala.collection.mutable.HashMap[IVariable, Variable]()
}

abstract class Scope(staticVars: List[Variable], parent: Scope, state: State) {
  private var varMap: List[Variable] = if (parent != null) {
    (staticVars.toSet ++ parent.varMap.toSet).toList
  } else {
    staticVars.toSet.toList
  }
  val pathStack = new Stack[NodePath]()

  val stack = new Stack[ValueType]()
  val startingStackAddr = state.stackInsertIndex

  def resolveId(name: IASTNode): Variable = {
    varMap.find{_.name == name.getRawSignature}.getOrElse(state.functionPointers(name.getRawSignature))
  }

  def resolveId(name: String): Variable = {
    varMap.find{_.name == name}.getOrElse(state.functionPointers(name))
  }

  def addArrayVariable(name: IASTNode, theType: IArrayType, dimensions: Seq[Int]): ArrayVariable = {

    if (!staticVars.exists{_.name == name.getRawSignature}) {
      val newVar = new ArrayVariable(name.getRawSignature, state, theType, dimensions)
      varMap = varMap.filter { theVar => theVar.name != name.getRawSignature } :+ newVar
      newVar
    } else {
      staticVars.find{_.name == name.getRawSignature}.get.asInstanceOf[ArrayVariable]
    }
  }

  def addVariable(name: IASTNode, theType: IType): Variable = {

    if (!staticVars.exists{_.name == name.getRawSignature}) {
      val newVar = new Variable(name.getRawSignature, state, theType)
      varMap = varMap.filter { theVar => theVar.name != name.getRawSignature } :+ newVar
      newVar
    } else {
      staticVars.find{_.name == name.getRawSignature}.get
    }
  }
}