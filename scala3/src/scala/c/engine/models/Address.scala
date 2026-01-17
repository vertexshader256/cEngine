package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.IType

import scala.c.engine.cEngVal

case class Address(location: Int, segment: Memory) {
	def +(offset: Int) = Address(location + offset, segment)
	def -(offset: Int) = Address(location - offset, segment)
}
