package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.{IBasicType, IType}
import org.eclipse.cdt.internal.core.dom.parser.c.{CBasicType, CPointerType}
import scala.c.engine.*

case class StringLiteral(value: String) extends ValueType {
	val theType: IType = CPointerType(TypeHelper.charType, 0)
	val rawType: IType = theType
}
