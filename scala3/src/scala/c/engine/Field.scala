package scala.c.engine

import org.eclipse.cdt.core.dom.ast.IType

case class Field(state: State, address: Int, bitOffset: Int, theType: IType, sizeInBits: Int) extends LValue {
	val sizeof = sizeInBits / 8
	val rawType = theType
}
