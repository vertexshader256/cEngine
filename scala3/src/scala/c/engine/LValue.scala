package scala.c.engine

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

import java.io.File
import java.nio.file.{Files, Paths}

// LValue is an memory location which identifies an object and has a type and various other attributes
trait LValue extends ValueType {
	val address: Int
	val theType: IType
	val bitOffset: Int
	val state: State
	val sizeInBits: Int

	private var rVal: RValue = RValue(0, TypeHelper.intType)

	def sizeof: Int

	def rValue: RValue = {
		if rVal.isInstanceOf[FileRValue] then
			rVal
		else if TypeHelper.isPointerOrArray(this) then
			Address(getValue.value.asInstanceOf[Int], TypeHelper.getPointerType(theType))
		else
			RValue(getValue.value, theType)
	}

	private def getValue = if (theType.isInstanceOf[IArrayType]) {
		RValue(address, theType)
	} else {
		state.Stack.readFromMemory(address, theType, bitOffset, sizeInBits)
	}

	def setValue(newVal: RValue) = {
		rVal = newVal
		state.Stack.writeToMemory(newVal.value, address, theType, bitOffset, sizeInBits)
	}

	def toByteArray = state.readDataBlock(address, sizeof)(using state)
}

object LValue {
	def unapply(info: LValue): Option[(Int, IType)] = Some((info.address, info.theType))

	def apply(theState: State, addr: Int, aType: IType) =
		new LValue {
			val address = addr
			val state = theState
			val bitOffset = 0
			val theType = TypeHelper.stripSyntheticTypeInfo(aType)
			val rawType = aType
			//def sizeof = TypeHelper.sizeof(theType)(state)}
			val sizeof = {
				TypeHelper.getPointerSize(theType)(using state)
			}
			val sizeInBits = sizeof * 8
		}
}
