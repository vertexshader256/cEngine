package scala.c.engine.gcc

import java.io.{File, InputStream, OutputStream, PrintWriter}
import scala.c.engine.models.NumBits.*
import scala.collection.mutable.ListBuffer
import scala.sys.process.ProcessIO
import scala.sys.process.Process
import scala.c.engine.models.*
import scala.c.engine.*

class RunLogger {
	def process = ProcessIO(in, out, err)

	val stdout = ListBuffer[String]()

	def recordStdOut(lines: Seq[String]) = {
		stdout ++= lines
	}

	def in(stream: OutputStream) = {}
	def out(stream: InputStream) = {
		recordStdOut(scala.io.Source.fromInputStream(stream).getLines().toSeq)
	}

	def err(stream: InputStream) = {
		//val lines = scala.io.Source.fromInputStream(stream).getLines.toSeq
	}
}
