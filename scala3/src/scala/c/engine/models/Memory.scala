package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.{CEnumeration, CStructure, CTypedef}
import java.math.BigInteger
import java.nio.{ByteBuffer, ByteOrder}
import java.util
import scala.c.engine.*
import java.nio.{ByteBuffer, ByteOrder}
import scala.c.engine.*

class Memory(size: Int, name: String) {

	import org.eclipse.cdt.core.dom.ast.IBasicType.Kind.*

	private var insertIndex = 0

	override def toString: String = name

	def setInsertIndex(pos: Int) = {
		insertIndex = pos
	}

	def getInsertIndex: Int = insertIndex

	private val tape: ByteBuffer = ByteBuffer.allocateDirect(size)
	tape.order(ByteOrder.LITTLE_ENDIAN)

	def writeDataBlock(array: Array[Byte], startingAddress: Int): Unit = {
		tape.mark()
		tape.position(startingAddress)
		tape.put(array, 0, array.length)
		tape.reset
	}

	def readDataBlock(startingAddress: Int, length: Int): Array[Byte] = {
		val result = new Array[Byte](length)
		tape.mark()
		tape.position(startingAddress)
		tape.get(result, 0, length)
		tape.reset
		result
	}

	def copy(dst: Int, src: Int, numBytes: Int): Unit = {
		tape.mark()
		tape.position(src)
		val array = new Array[Byte](numBytes)
		tape.get(array)
		tape.position(dst)
		tape.put(array)
		tape.reset()
	}

	// fills a destination with a value, numBytes worth
	def set(dst: Int, value: Byte, numBytes: Int): Unit = {
		val array = new Array[Byte](numBytes)
		util.Arrays.fill(array, value)
		tape.mark()
		tape.position(dst)
		tape.put(array)
		tape.reset()
	}

	def readPtrVal(address: Int): Int = {
		tape.getInt(address)
	}

	def clearMemory(startingAddress: Int, numBytes: Int): Unit = {
		var address = startingAddress
		for (i <- 0 until numBytes) {
			tape.put(address, 0.toByte)
			address += 1
		}
	}

	def putShort(address: Int, short: Short): Unit = {
		tape.putShort(address, short)
	}

	def getShort(address: Int): Short = {
		tape.getShort(address)
	}

	def putLong(address: Int, long: Long): Unit = {
		tape.putLong(address, long)
	}

	def getLong(address: Int): Long = {
		tape.getLong(address)
	}

	def getLongLong(address: Int): BigInt = {
		val bytes = Array[Byte](8)
		tape.get(address, bytes)
		BigInteger(bytes)
	}

	def putInt(address: Int, int: Int): Unit = {
		tape.putInt(address, int)
	}

	def getInt(address: Int): Int = {
		tape.getInt(address)
	}

	def putByte(address: Int, byte: Byte): Unit = {
		tape.put(address, byte)
	}

	def getByte(address: Int): Byte = {
		tape.get(address)
	}

	def putDouble(address: Int, double: Double): Unit = {
		tape.putDouble(address, double)
	}

	def getDouble(address: Int): Double = {
		tape.getDouble(address)
	}

	def putFloat(address: Int, float: Float): Unit = {
		tape.putFloat(address, float)
	}

	def getFloat(address: Int): Float = {
		tape.getFloat(address)
	}

	def writePointerToMemory(newVal: cEngVal, address: Int): Unit = {
		newVal match {
			case int: Int => putInt(address, int)
			case long: Long => putInt(address, long.toInt)
		}
	}

	def allocate(numBytes: Int): Address = {
		val result = insertIndex
		insertIndex += Math.max(0, numBytes)
		Address(result)
	}

	private def writeInteger(newVal: cEngVal, address: Int, bitOffset: Int = 0, sizeInBits: Int = 0) = {
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

	private def writeLongLong(newVal: cEngVal, address: Int, isUnsigned: Boolean, bitOffset: Int = 0, sizeInBits: Int = 0) = {
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
	def writeToMemory(newVal: cEngVal, address: Int, theType: IType, bitOffset: Int = 0, sizeInBits: Int = 0): Unit = {

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
					case char: char => putByte(address, char)
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

	def readFromMemoryRaw(basic: IBasicType, address: Int, bitOffset: Int = 0, sizeInBits: Int = 0): cEngVal = {
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

	def readFromMemory(address: Int, theType: IType, bitOffset: Int = 0, sizeInBits: Int = 0): RValue = {
		theType match
			case basic: IBasicType =>
				val value = readFromMemoryRaw(basic, address, bitOffset, sizeInBits)
				TypeHelper.castSign(basic, value)
			case typedef: CTypedef => readFromMemory(address, typedef.getType)
			case _ => RValue(getInt(address), theType)
	}
}
