package scala.c.engine

import org.eclipse.cdt.core.dom.ast.IType

import java.io.File
import java.nio.file.{Files, Paths}

case class FileRValue(path: String) extends RValue {

	val theType: IType = null
	val rawType: IType = theType

	val file: File = new File(path)

	val value: cEngVal = if file.exists then 1 else 0
	var isOpen = true

	var byteArray = if file.exists then
		Files.readAllBytes(Paths.get(path))
	else
		Array[Byte]()

	var currentPosition = 0

	def close(): Boolean = {
		isOpen = false
		true
	}

	def read(numBytes: Int): Array[Byte] = {
		if isOpen then
			val result = byteArray.drop(currentPosition).take(numBytes)
			currentPosition += numBytes
			result
		else
			Array()
	}

	def write(bytes: Array[Byte], numBytes: Int) = {
		val head = byteArray.take(currentPosition)
		val tail = byteArray.drop(currentPosition)

		head ++ bytes ++ tail

		currentPosition += numBytes
	}

	def printf(str: String) = {
		import java.io._
		val pw = new PrintWriter(file)
		pw.write(str)
		pw.close

		byteArray ++= str.getBytes
	}
}
