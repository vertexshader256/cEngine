package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.IType

import scala.c.engine.cEngVal

case class Address(location: Int) {
	def +(offset: Int) = Address(location + offset)
	def -(offset: Int) = Address(location - offset)
}
