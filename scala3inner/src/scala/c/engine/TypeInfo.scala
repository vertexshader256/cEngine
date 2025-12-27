package scala.c.engine

import org.eclipse.cdt.core.dom.ast.IType

case class TypeInfo(value: IType) extends ValueType {
	val theType: IType = value
	val rawType: IType = theType
}
