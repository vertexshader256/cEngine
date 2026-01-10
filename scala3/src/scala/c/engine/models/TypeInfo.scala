package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.IType
import scala.c.engine.models.*

case class TypeInfo(value: IType) extends ValueType {
	val theType: IType = value
	val rawType: IType = theType
}
