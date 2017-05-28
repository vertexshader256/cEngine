package scala.c.engine

import c.engine.{ArrayVariable, State, ValueType, Variable}
import org.eclipse.cdt.core.dom.ast.{IASTLabelStatement, IASTNode, IArrayType, IType}

import scala.collection.mutable.{ListBuffer, Stack}

class ExecutionContext(staticVars: List[Variable], parentScopeVars: List[Variable], val returnType: IType, val startingStackAddr: Int, state: State) {
  val visited = new ListBuffer[IASTNode]()
  var varMap: List[Variable] = (staticVars.toSet ++ parentScopeVars.toSet).toList
  val pathStack = new Stack[IASTNode]()
  val labels = new ListBuffer[(IASTLabelStatement, Stack[IASTNode], List[IASTNode])]()
  val stack = new Stack[ValueType]()

  def resolveId(id: String): Variable = varMap.find{_.name == id}.getOrElse(state.functionPointers(id))

  def addArrayVariable(name: String, theType: IArrayType, dimensions: Seq[Int]): ArrayVariable = {

    if (!staticVars.exists{_.name == name}) {
      val newVar = new ArrayVariable(name, state, theType, dimensions)
      varMap = varMap.filter { theVar => theVar.name != name } :+ newVar
      newVar
    } else {
      staticVars.find{_.name == name}.get.asInstanceOf[ArrayVariable]
    }
  }

  def addVariable(name: String, theType: IType): Variable = {

    if (!staticVars.exists{_.name == name}) {
      val newVar = new Variable(name, state, theType)
      varMap = varMap.filter { theVar => theVar.name != name } :+ newVar
      newVar
    } else {
      staticVars.find{_.name == name}.get
    }
  }
}