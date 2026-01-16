package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.{IASTName, IArrayType, IType}
import org.eclipse.cdt.core.model.IVariable

import scala.c.engine.*

object Variable {
	def apply(name: IASTName, state: State, aType: IType, initVals: List[RValue]): Variable = {

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
		state.writeDataBlock(initVals, variable.address)
		variable
	}

	def apply(name: IASTName, state: State, aType: IType): Variable = {
		val size = TypeHelper.sizeof(aType)(using state)
		Variable(name, state, aType, size)
	}
}

case class Variable(theName: IASTName, state: State, aType: IType, sizeof: Int) extends LValue {

	val theType = TypeHelper.stripSyntheticTypeInfo(aType)
	val rawType = aType
	val bitOffset = 0
	val sizeInBits = sizeof * 8
	val name = theName.toString

	// need this for function-scoped static vars
	var isInitialized = false

	val isStatic = {
		val binding = theName.resolveBinding()
		binding match
			case vari: IVariable => vari.isStatic
			case _ => false
	}

	val segment = if !isStatic then
		state.Stack
	else
		state.dataSegment

	val address = segment.allocate(sizeof)

	override def rValue: RValue = {
		if rVal.isInstanceOf[FileRValue] then
			rVal
		else if TypeHelper.isPointerOrArray(this) then
			Pointer(getValue.value.asInstanceOf[Int], TypeHelper.getPointerType(theType))
		else
			RValue(getValue.value, theType)
	}

	private def getValue = if (theType.isInstanceOf[IArrayType]) {
		RValue(address, theType)
	} else {
		segment.readFromMemory(address, theType, bitOffset, sizeInBits)
	}

	override def toString = {
		"Variable(" + name + ", " + address + ", " + theType.getClass.getSimpleName + ")"
	}
}
