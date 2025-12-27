package scala.c.engine

import org.eclipse.cdt.core.dom.ast.IType

abstract class ValueType {
	def theType: IType
	def rawType: IType
}
