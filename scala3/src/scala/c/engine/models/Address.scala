package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.IType

import scala.c.engine.cEngVal

case class Address(location: Int, segment: Memory) {
	def +(offset: Int) = Address(location + offset, segment)
	def -(offset: Int) = Address(location - offset, segment)

	def writeToMemory(newVal: cEngVal, theType: IType, bitOffset: Int = 0, sizeInBits: Int = 0) = {
		segment.writeToMemory(newVal, location, theType)
	}
}
