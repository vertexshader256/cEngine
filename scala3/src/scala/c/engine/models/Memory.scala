package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.{CEnumeration, CStructure, CTypedef}

import java.math.BigInteger
import java.nio.{ByteBuffer, ByteOrder}
import java.util
import scala.c.engine.*

class Memory(stackSize: Int, dataSize: Int, heapSize: Int) {

	import org.eclipse.cdt.core.dom.ast.IBasicType.Kind.*

	private var dataInsertIndex = 10 // dont start at address 0, as that is a null pointer
	private var insertIndex = dataSize // stack
	private var heapInsertIndex = dataSize + stackSize

	def setStackPosition(pos: Int) = {
		insertIndex = pos
	}

	def getStackPosition: Int = insertIndex

	private val tape: ByteBuffer = ByteBuffer.allocateDirect(dataSize + stackSize + heapSize)
	tape.order(ByteOrder.LITTLE_ENDIAN)

	def writeDataBlock(array: Array[Byte], startingAddress: Address): Unit = {
		tape.mark()
		tape.position(startingAddress.location)
		tape.put(array, 0, array.length)
		tape.reset
	}

	def readDataBlock(startingAddress: Address, length: Int): Array[Byte] = {
		val result = new Array[Byte](length)
		tape.mark()
		tape.position(startingAddress.location)
		tape.get(result, 0, length)
		tape.reset
		result
	}

	// fills a destination with a value, numBytes worth
	def set(dst: Address, value: Byte, numBytes: Int): Unit = {
		val array = new Array[Byte](numBytes)
		util.Arrays.fill(array, value)
		tape.mark()
		tape.position(dst.location)
		tape.put(array)
		tape.reset()
	}

	def readPtrVal(address: Address): Int = {
		tape.getInt(address.location)
	}

	def clearMemory(startingAddress: Address, numBytes: Int): Unit = {
		var address = startingAddress.location
		for (i <- 0 until numBytes) {
			tape.put(address, 0.toByte)
			address += 1
		}
	}

	def putShort(address: Address, short: Short): Unit = {
		tape.putShort(address.location, short)
	}

	def getShort(address: Address): Short = {
		tape.getShort(address.location)
	}

	def putLong(address: Address, long: Long): Unit = {
		tape.putLong(address.location, long)
	}

	def getLong(address: Address): Long = {
		tape.getLong(address.location)
	}

	def putInt(address: Address, int: Int): Unit = {
		tape.putInt(address.location, int)
	}

	def getInt(address: Address): Int = {
		tape.getInt(address.location)
	}

	def putByte(address: Address, byte: Byte): Unit = {
		tape.put(address.location, byte)
	}

	def getByte(address: Address): Byte = {
		tape.get(address.location)
	}

	def putDouble(address: Address, double: Double): Unit = {
		tape.putDouble(address.location, double)
	}

	def getDouble(address: Address): Double = {
		tape.getDouble(address.location)
	}

	def putFloat(address: Address, float: Float): Unit = {
		tape.putFloat(address.location, float)
	}

	def getFloat(address: Address): Float = {
		tape.getFloat(address.location)
	}

	def writePointerToMemory(newVal: cEngVal, address: Address): Unit = {
		newVal match {
			case int: Int => putInt(address, int)
			case long: Long => putInt(address, long.toInt)
		}
	}

	def allocateData(numBytes: Int): Address = {
		val result = dataInsertIndex
		dataInsertIndex += Math.max(0, numBytes)
		Address(result)
	}

	def allocate(numBytes: Int): Address = {
		val result = insertIndex
		insertIndex += Math.max(0, numBytes)
		Address(result)
	}

	def allocateHeapSpace(numBytes: Int): Address = {
		val result = heapInsertIndex
		heapInsertIndex += Math.max(0, numBytes)
		Address(result)
	}

	private def writeInteger(newVal: cEngVal, address: Address, bitOffset: Int, sizeInBits: Int) = {
		newVal match {
			case int: Int =>
				val x = if (bitOffset != 0) {
					val currentVal = getInt(address)
					val right = currentVal << (32 - bitOffset) >>> (32 - bitOffset)
					val left = currentVal >> (sizeInBits + bitOffset) << (sizeInBits + bitOffset)

					val newVal = int << bitOffset
					left + newVal + right
				} else {
					int
				}

				putInt(address, x)
			case long: Long => putInt(address, long.toInt)
		}
	}

	private def writeLongLong(newVal: cEngVal, address: Address, isUnsigned: Boolean, bitOffset: Int, sizeInBits: Int) = {
		newVal match
			case long: Long => putLong(address, long)
			case int: Int => putInt(address, int)
			case bigInt: BigInt =>
				if isUnsigned then
					val unsigned = java.lang.Long.parseUnsignedLong(bigInt.toString)
					putLong(address, unsigned)
				else
					putLong(address, bigInt.toLong)
	}

	// use Address type to prevent messing up argument order
	def writeToMemory(newVal: cEngVal, address: Address, theType: IType, bitOffset: Int = 0, sizeInBits: Int = 0): Unit = {

		TypeHelper.stripSyntheticTypeInfo(theType) match {
			case basic: IBasicType if basic.isShort =>
				newVal match
					case int: Int => putShort(address, int.asInstanceOf[Short])
					case short: Short => putShort(address, short)
			case basic: IBasicType if basic.isLongLong =>
				writeLongLong(newVal, address, basic.isUnsigned, bitOffset, sizeInBits)
			case basic: IBasicType if basic.isLong =>
				newVal match
					case long: Long => putInt(address, long.toInt)
			case _: CEnumeration =>
				newVal match
					case int: Int => putInt(address, int)
					case long: Long => putInt(address, long.toInt)
					case short: Short => putInt(address, short.toInt)
					case byte: Byte => putInt(address, byte.toInt)
			case basic: IBasicType if basic.getKind == eInt =>
				writeInteger(newVal, address, bitOffset, sizeInBits)
			case basic: IBasicType if basic.getKind == eDouble || basic.getKind == eFloat =>
				newVal match
					case double: Double => putDouble(address, double)
					case float: Float => putFloat(address, float)
			case basic: IBasicType if basic.getKind == eChar =>
				newVal match
					case char: Byte => putByte(address, char)
					case int: Int => putByte(address, int.toByte)
			case basic: IBasicType =>
				putInt(address, newVal.asInstanceOf[Int])
			case _: IFunctionType =>
				writePointerToMemory(newVal, address)
			case _: CStructure =>
				writePointerToMemory(newVal, address)
			case _: IPointerType =>
				writePointerToMemory(newVal, address)
			case _: IArrayType =>
				writePointerToMemory(newVal, address)
		}
	}

	def readFromMemoryRaw(basic: IBasicType, address: Address, bitOffset: Int = 0, sizeInBits: Int = 0): cEngVal = {
		if basic.isShort then
			val result = getShort(address)
			(result << (16 - sizeInBits - bitOffset) >> (16 - sizeInBits)).toShort
		else if basic.getKind == eInt && basic.isLongLong then
			val result = getLong(address)
			result << (64 - sizeInBits - bitOffset) >> (64 - sizeInBits)
		else if basic.getKind == eInt || basic.getKind == eBoolean then
			val result = getInt(address)
			result << (32 - sizeInBits - bitOffset) >>> (32 - sizeInBits)
		else if basic.getKind == eDouble then
			getDouble(address)
		else if basic.getKind == eFloat then
			getFloat(address)
		else
			getByte(address) // a C 'char' is a Java 'byte'
	}

	def readFromMemory(address: Address, theType: IType, bitOffset: Int = 0, sizeInBits: Int = 0): RValue = {
		theType match
			case basic: IBasicType =>
				val value = readFromMemoryRaw(basic, address, bitOffset, sizeInBits)
				TypeHelper.castSign(basic, value)
			case typedef: CTypedef => readFromMemory(address, typedef.getType)
			case _ => RValue(getInt(address), theType)
	}
}
