package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.IType
import scala.c.engine.*

case class Field(state: State, address: Int, bitOffset: Int, theType: IType, sizeInBits: Int) extends LValue {
	val sizeof = sizeInBits / 8
	val rawType = theType
}
