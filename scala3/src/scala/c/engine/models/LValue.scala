package scala
package c
package engine
package models

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import java.io.File
import java.nio.file.{Files, Paths}
import scala.c.engine.TypeHelper
import scala.c.engine.models.*

// LValue is an memory location which identifies an object and has a type and various other attributes
trait LValue extends ValueType {
	val address: Address
	val theType: IType
	val bitOffset: Int
	val state: State
	val sizeInBits: Int

	final protected var rVal: RValue = RValue(0, TypeHelper.intType)

	def sizeof: Int

	def rValue: RValue = {
		if rVal.isInstanceOf[FileRValue] then
			rVal
		else if TypeHelper.isPointerOrArray(this) then {
			val address = Address(getValue.value.asInstanceOf[Int], state.stack)
			Pointer(address, TypeHelper.getPointerType(theType))
		} else
			RValue(getValue.value, theType)
	}

	private def getValue = if (theType.isInstanceOf[IArrayType]) {
		RValue(address.location, theType)
	} else {
		address.readFromMemory(theType, bitOffset, sizeInBits)
	}

	def setValue(newVal: RValue): Unit = {
		rVal = newVal
		address.segment.writeToMemory(newVal.value, address.location, theType, bitOffset, sizeInBits)
	}

	def toByteArray: Array[Byte] = state.readDataBlock(Address(address.location, state.stack), sizeof)
}

object LValue {
	def unapply(info: LValue): Option[(Address, IType)] = Some((info.address, info.theType))

	def apply(theState: State, addr: Address, aType: IType) =
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
