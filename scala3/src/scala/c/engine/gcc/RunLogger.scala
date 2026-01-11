package scala.c.engine.gcc

import java.io.{InputStream, OutputStream}
import scala.collection.mutable.ListBuffer
import scala.sys.process.ProcessIO

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
