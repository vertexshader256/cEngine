package scala.c.engine.gcc

import java.io.{InputStream, OutputStream}
import scala.collection.mutable.ListBuffer

class LinkerLogger extends Logger {
	val errors = ListBuffer[String]()

	def addErrors(newErrors: Seq[String]) = {
		errors ++= newErrors
	}

	def in(stream: OutputStream) = {}
	def out(stream: InputStream) = {
		scala.io.Source.fromInputStream(stream).getLines()
	}

	def err(stream: InputStream) = {

		val lines = scala.io.Source.fromInputStream(stream).getLines().toSeq

		if (lines.nonEmpty) {
			lines.foreach(x => println("LINKER ERROR: " + x))
		}
	}
}
