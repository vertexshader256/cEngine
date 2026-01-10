package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.IType
import scala.c.engine.*

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
