package scala.c.engine

import org.eclipse.cdt.core.dom.ast.{IArrayType, IBasicType, IFunctionType, IPointerType, IType}
import org.eclipse.cdt.internal.core.dom.parser.c.{CEnumeration, CStructure, CTypedef}

class Memory(size: Int) {

	import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._

	var insertIndex = 0
	// turing tape
	val tape = new Tape(size)

	// use Address type to prevent messing up argument order
	def writeToMemory(newVal: cEngVal, address: Int, theType: IType, bitOffset: Int = 0, sizeInBits: Int = 0): Unit = {

		TypeHelper.stripSyntheticTypeInfo(theType) match {
			case basic: IBasicType if basic.isShort =>
				newVal match
					case int: Int => tape.putShort(address, int.asInstanceOf[Short])
					case short: Short => tape.putShort(address, short)
			case basic: IBasicType if basic.isLongLong =>
				newVal match
					case long: Long => tape.putLong(address, long)
					case int: Int => tape.putInt(address, int)
					case bigInt: BigInt =>
						if basic.isUnsigned then
							val unsigned = java.lang.Long.parseUnsignedLong(bigInt.toString)
							tape.putLong(address, unsigned)
						else
							tape.putLong(address, bigInt.toLong)
			case basic: IBasicType if basic.isLong =>
				newVal match
					case long: Long => tape.putInt(address, long.toInt)
			case _: CEnumeration =>
				newVal match
					case int: Int => tape.putInt(address, int)
			case basic: IBasicType if basic.getKind == eInt =>
				newVal match {
					case int: Int =>
						val x = if (bitOffset != 0) {
							val currentVal = tape.getInt(address)
							val right = currentVal << (32 - bitOffset) >>> (32 - bitOffset)
							val left = currentVal >> (sizeInBits + bitOffset) << (sizeInBits + bitOffset)

							val newVal = int << bitOffset
							left + newVal + right
						} else {
							int
						}

						tape.putInt(address, x)
					case long: Long => tape.putInt(address, long.toInt)
				}
			case basic: IBasicType if basic.getKind == eDouble || basic.getKind == eFloat =>
				newVal match
					case double: Double => tape.putDouble(address, double)
					case float: Float => tape.putFloat(address, float)
			case basic: IBasicType if basic.getKind == eChar =>
				newVal match
					case char: char => tape.putByte(address, char)
					case int: Int => tape.putByte(address, int.toByte)
			case basic: IBasicType =>
				tape.putInt(address, newVal.asInstanceOf[Int])
			case _: IFunctionType =>
				tape.writePointerToMemory(newVal, address)
			case _: CStructure =>
				tape.writePointerToMemory(newVal, address)
			case _: IPointerType =>
				tape.writePointerToMemory(newVal, address)
			case _: IArrayType =>
				tape.writePointerToMemory(newVal, address)
		}
	}

	def readFromMemoryRaw(basic: IBasicType, address: Int, bitOffset: Int = 0, sizeInBits: Int = 0): cEngVal = {
		if basic.isShort then
			val result = tape.getShort(address)
			(result << (16 - sizeInBits - bitOffset) >> (16 - sizeInBits)).toShort
		else if basic.getKind == eInt && basic.isLongLong then
			val result = tape.getLong(address)
			result << (64 - sizeInBits - bitOffset) >> (64 - sizeInBits)
		else if basic.getKind == eInt || basic.getKind == eBoolean then
			val result = tape.getInt(address)
			result << (32 - sizeInBits - bitOffset) >>> (32 - sizeInBits)
		else if basic.getKind == eDouble then
			tape.getDouble(address)
		else if basic.getKind == eFloat then
			tape.getFloat(address)
		else
			tape.getByte(address) // a C 'char' is a Java 'byte'
	}

	def readFromMemory(address: Int, theType: IType, bitOffset: Int = 0, sizeInBits: Int = 0): RValue = {
		theType match
			case basic: IBasicType =>
				val value = readFromMemoryRaw(basic, address, bitOffset, sizeInBits)
				TypeHelper.castSign(basic, value)
			case typedef: CTypedef => readFromMemory(address, typedef.getType)
			case _ => RValue(tape.getInt(address), theType)
	}
}
