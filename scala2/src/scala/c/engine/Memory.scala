package scala.c.engine

import org.eclipse.cdt.core.dom.ast.{IArrayType, IBasicType, IFunctionType, IPointerType, IType}
import org.eclipse.cdt.internal.core.dom.parser.c.{CEnumeration, CStructure, CTypedef}

import java.nio.{ByteBuffer, ByteOrder}
import java.util

class Memory(size: Int) {

	import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._

	var insertIndex = 0
	// turing tape
	private val tape = ByteBuffer.allocateDirect(size)
	tape.order(ByteOrder.LITTLE_ENDIAN)

	def writeDataBlock(array: Array[Byte], startingAddress: Int): Unit = {
		tape.mark()
		tape.position(startingAddress)
		tape.put(array, 0, array.size)
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

	def copy(dst: Int, src: Int, numBytes: Int) = {
		tape.mark()
		tape.position(src)
		val array = new Array[Byte](numBytes)
		tape.get(array)
		tape.position(dst)
		tape.put(array)
		tape.reset()
	}

	def set(dst: Int, value: Byte, numBytes: Int) = {
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

	def clearMemory(startingAddress: Int, numBytes: Int) = {
		var address = startingAddress
		for (i <- 0 until numBytes) {
			tape.put(address, 0.toByte)
			address += 1
		}
	}

	// use Address type to prevent messing up argument order
	def writeToMemory(newVal: AnyVal, address: Int, theType: IType, bitOffset: Int = 0, sizeInBits: Int = 0): Unit = {

		TypeHelper2.stripSyntheticTypeInfo(theType) match {
			case basic: IBasicType if basic.getKind == eInt && basic.isShort =>
				newVal match {
					case int: Int => tape.putShort(address, int.asInstanceOf[Short])
					case short: Short => tape.putShort(address, short)
				}
			case basic: IBasicType if basic.getKind == eInt && basic.isLongLong =>
				newVal match {
					case long: Long => tape.putLong(address, long)
					case int: Int => tape.putInt(address, int)
				}
			case basic: IBasicType if basic.getKind == eInt && basic.isLong =>
				newVal match {
					case long: Long => tape.putInt(address, long.toInt)
				}
			case _: CEnumeration =>
				newVal match {
					case int: Int => tape.putInt(address, int)
				}
			case basic: IBasicType if basic.getKind == eInt || basic.getKind == eVoid =>
				newVal match {
					case int: Int =>
						val x = if (bitOffset != 0) {
							val currentVal = tape.getInt(address)
							val right = currentVal << (32 - bitOffset) >>> (32 - bitOffset)
							val left = currentVal >>> (sizeInBits + bitOffset) << (sizeInBits + bitOffset)

							val newVal = int << bitOffset
							left + newVal + right
						} else {
							int
						}

						tape.putInt(address, x)
					case long: Long => tape.putInt(address, long.toInt)
				}
			case basic: IBasicType if basic.getKind == eDouble =>
				newVal match {
					case double: Double => tape.putDouble(address, double)
				}
			case basic: IBasicType if basic.getKind == eFloat =>
				newVal match {
					case float: Float => tape.putFloat(address, float)
				}
			case basic: IBasicType if basic.getKind == eChar =>
				newVal match {
					case char: char => tape.put(address, char)
					case int: Int => tape.put(address, int.toByte)
				}
			case basic: IBasicType if basic.getKind == eBoolean =>
				tape.putInt(address, newVal.asInstanceOf[Int])
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

	private def writePointerToMemory(newVal: AnyVal, address: Int) = {
		newVal match {
			case int: Int => tape.putInt(address, int)
			case long: Long => tape.putInt(address, long.toInt)
		}
	}

	def readFromMemory(address: Int, theType: IType, bitOffset: Int = 0, sizeInBits: Int = 0): RValue = {
		theType match {
			case basic: IBasicType =>
				val res = if (basic.getKind == eInt && basic.isShort) {
					val result = tape.getShort(address)
					(result << (16 - sizeInBits - bitOffset) >>> (16 - sizeInBits)).toShort
				} else if (basic.getKind == eInt && basic.isLongLong) {
					val result = tape.getLong(address)
					result << (64 - sizeInBits - bitOffset) >>> (64 - sizeInBits)
				} else if (basic.getKind == eInt || basic.getKind == eBoolean) {
					val result = tape.getInt(address)
					result << (32 - sizeInBits - bitOffset) >>> (32 - sizeInBits)
				} else if (basic.getKind == eDouble) {
					tape.getDouble(address)
				} else if (basic.getKind == eFloat) {
					tape.getFloat(address)
				} else if (basic.getKind == eChar) {
					tape.get(address) // a C 'char' is a Java 'byte'
				}

				TypeHelper2.castSign(theType, res)
			case typedef: CTypedef => readFromMemory(address, typedef.getType)
			case _ => RValue(tape.getInt(address), theType)
		}
	}
}
