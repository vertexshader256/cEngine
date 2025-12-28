package scala.c.engine

import java.nio.{ByteBuffer, ByteOrder}
import java.util

class Tape(size: Int) {
	val tape = ByteBuffer.allocateDirect(size)
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

	def putShort(address: Int, short: Short) = {
		tape.putShort(address, short)
	}

	def getShort(address: Int): Short = {
		tape.getShort(address)
	}

	def putLong(address: Int, long: Long) = {
		tape.putLong(address, long)
	}

	def getLong(address: Int): Long = {
		tape.getLong(address)
	}

	def putInt(address: Int, int: Int) = {
		tape.putInt(address, int)
	}

	def putByte(address: Int, byte: Byte) = {
		tape.put(address, byte)
	}

	def getByte(address: Int): Byte = {
		tape.get(address)
	}

	def getInt(address: Int): Int = {
		tape.getInt(address)
	}

	def putDouble(address: Int, double: Double) = {
		tape.putDouble(address, double)
	}

	def getDouble(address: Int): Double = {
		tape.getDouble(address)
	}

	def putFloat(address: Int, float: Float) = {
		tape.putFloat(address, float)
	}

	def getFloat(address: Int): Float = {
		tape.getFloat(address)
	}

	def writePointerToMemory(newVal: AnyVal, address: Int) = {
		newVal match {
			case int: Int => putInt(address, int)
			case long: Long => putInt(address, long.toInt)
		}
	}
}
