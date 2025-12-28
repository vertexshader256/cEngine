package scala.c.engine

import org.eclipse.cdt.core.dom.ast.{IBasicType, IType}
import org.eclipse.cdt.internal.core.dom.parser.c.{CBasicType, CPointerType}

case class StringLiteral(value: String) extends ValueType {
	val theType: IType = new CPointerType(new CBasicType(IBasicType.Kind.eChar, 0), 0)
	val rawType: IType = theType
}
