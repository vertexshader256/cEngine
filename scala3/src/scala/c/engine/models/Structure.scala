package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.{IArrayType, IType}

import scala.c.engine.*

case class Structure(name: String, state: State, aType: IType, sizeof: Int) extends LValue {

	val theType = TypeHelper.stripSyntheticTypeInfo(aType)
	val rawType = aType
	val bitOffset = 0
	val sizeInBits = sizeof * 8

	val address = state.allocateSpace(sizeof)

	// need this for function-scoped static vars
	var isInitialized = false

	override def toString = {
		"Structure(" + name + ", " + address + ", " + theType.getClass.getSimpleName + ")"
	}
}
