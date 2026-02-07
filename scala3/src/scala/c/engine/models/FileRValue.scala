package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.IType

import java.io.File
import java.nio.file.{Files, Paths}
import scala.c.engine.*

case class FileRValue(path: String) extends RValue {

	val theType: IType = null
	val rawType: IType = theType

	val file: File = File(path)

	val value: cEngVal = if file.exists then 1 else 0
	private var isOpen = true

	private var byteArray = if file.exists then
		Files.readAllBytes(Paths.get(path))
	else
		Array[Byte]()

	private var currentPosition = 0

	def close(): Boolean = {
		isOpen = false
		true
	}

	def read(numBytes: Int): Array[Byte] = {
		if isOpen then
			val result = byteArray.slice(currentPosition, currentPosition + numBytes)
			currentPosition += numBytes
			result
		else
			Array()
	}

	def write(bytes: Array[Byte]): Unit = {
		import java.io.*
		val str = new String(bytes)
		val pw = PrintWriter(file)
		pw.write(str)
		pw.close()

		byteArray ++= str.getBytes
	}

	def printf(str: String): Unit = {
		import java.io.*
		val pw = PrintWriter(file)
		pw.write(str)
		pw.close()

		byteArray ++= str.getBytes
	}
}
