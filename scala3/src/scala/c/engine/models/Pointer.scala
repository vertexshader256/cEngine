package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.IType

import scala.c.engine.*

case class Pointer(address: Address, theType: IType) extends RValue {
	val value = address.location

	override def toString: String = {
		"Pointer(" + value + ", " + theType + ")"
	}

	override def sizeof(implicit state: State): Int = {
		state.pointerSize.ptrSize
	}

	val rawType = theType
}
