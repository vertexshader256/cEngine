package scala.c.engine.gcc

import java.io.{InputStream, OutputStream}
import scala.collection.mutable.ListBuffer

class SyntaxLogger extends Logger {
	val errors = ListBuffer[String]()

	def addErrors(newErrors: Seq[String]) = {
		errors ++= newErrors
	}

	def in(stream: OutputStream) = {}
	def out(stream: InputStream) = {}
	def err(stream: InputStream) = {

		val lines = scala.io.Source.fromInputStream(stream).getLines().toList
		errors ++= lines
	}
}
