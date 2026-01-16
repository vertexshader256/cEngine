package scala.c.engine.models

case class Address(location: Int, segment: Memory) {
	def +(offset: Int) = Address(location + offset, segment)
	def -(offset: Int) = Address(location - offset, segment)
}
