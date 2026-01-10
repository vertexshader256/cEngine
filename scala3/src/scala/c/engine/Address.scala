package scala.c.engine

import org.eclipse.cdt.core.dom.ast.IType

case class Address(value: Int, theType: IType) extends RValue {
	override def toString = {
		"Address(" + value + ", " + theType + ")"
	}

	override def sizeof(implicit state: State) = {
		state.pointerSize match {
			case NumBits.SixtyFourBits => 8
			case NumBits.ThirtyTwoBits => 4
		}
	}

	val rawType = theType
}
