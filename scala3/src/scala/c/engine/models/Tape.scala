package scala.c.engine.models

import java.math.BigInteger
import java.nio.{ByteBuffer, ByteOrder}
import java.util
import scala.c.engine.*

class Tape(size: Int) {
	val tape: ByteBuffer = ByteBuffer.allocateDirect(size)
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
		val bytes = new Array[Byte](8)
		tape.get(address, bytes)
		new BigInteger(bytes)
	}

	def putInt(address: Int, int: Int): Unit = {
		tape.putInt(address, int)
	}

	def putByte(address: Int, byte: Byte): Unit = {
		tape.put(address, byte)
	}

	def getByte(address: Int): Byte = {
		tape.get(address)
	}

	def getInt(address: Int): Int = {
		tape.getInt(address)
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
}
