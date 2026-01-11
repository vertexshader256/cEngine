package scala.c.engine.gcc

import java.io.{InputStream, OutputStream}
import scala.collection.mutable.ListBuffer
import scala.sys.process.ProcessIO

abstract class Logger {
	def in(stream: OutputStream): Unit

	def out(stream: InputStream): Unit

	def err(stream: InputStream): Unit

	def process = new ProcessIO(in, out, err)

	val availableErrors = ListBuffer[String]()
	var errorIsInHeaderFile = false
	val errorSource = ListBuffer[String]()
	var isParsingSourcePath = false
	var currentFunction = ""

	def addErrors(errors: Seq[String]): Unit

	def getErrors(lines: Seq[String]): Seq[String] = {
		lines
	}
}
