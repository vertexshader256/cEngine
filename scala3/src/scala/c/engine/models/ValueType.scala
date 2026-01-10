package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.IType
import scala.c.engine.*

abstract class ValueType {
	def theType: IType
	def rawType: IType
}
