package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.IType

import scala.c.engine.*

case class Address(value: Int, theType: IType) extends RValue {
	override def toString: String = {
		"Address(" + value + ", " + theType + ")"
	}

	override def sizeof(implicit state: State): Int = {
		state.pointerSize.ptrSize
	}

	val rawType = theType
}
