package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.{IArrayType, IType}
import scala.c.engine.*

object Variable {
	def apply(name: String, state: State, aType: IType, initVals: List[RValue]): Variable = {

		val size = if (aType.isInstanceOf[IArrayType] && initVals.nonEmpty) {
			if (aType.asInstanceOf[IArrayType].hasSize) {
				if initVals.size == aType.asInstanceOf[IArrayType].getSize.numericalValue().toInt then
					initVals.map { init => TypeHelper.sizeof(init.theType)(using state) }.sum
				else
					TypeHelper.sizeof(aType)(using state)
			} else {
				initVals.map { init => TypeHelper.sizeof(init.theType)(using state) }.sum
			}
		} else {
			TypeHelper.sizeof(aType)(using state)
		}

		val variable = Variable(name, state, aType, size)

		// now, write the initial values
		state.writeDataBlock(initVals, variable.address)(using state)
		variable
	}

	def apply(name: String, state: State, aType: IType): Variable = {
		val size = TypeHelper.sizeof(aType)(using state)
		Variable(name: String, state: State, aType: IType, size)
	}
}

case class Variable(name: String, state: State, aType: IType, sizeof: Int) extends LValue {

	val theType = TypeHelper.stripSyntheticTypeInfo(aType)
	val rawType = aType
	val bitOffset = 0
	val sizeInBits = sizeof * 8

	val address = state.allocateSpace(sizeof)

	// need this for function-scoped static vars
	var isInitialized = false

	override def toString = {
		"Variable(" + name + ", " + address + ", " + theType + ")"
	}
}
