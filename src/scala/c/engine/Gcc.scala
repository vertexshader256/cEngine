package scala.c.engine

import scala.sys.process.ProcessIO
import scala.sys.process._
import java.io.File

import scala.collection.mutable.ListBuffer
import java.io.{InputStream, OutputStream, PrintWriter}
import java.util.concurrent.atomic.AtomicInteger

import org.eclipse.cdt.core.dom.ast.IASTNode

object Gcc {

  val program = new FunctionScope(List(), null, null) {}

  def runCode(code: String, state: State) = {
      val exeCode = s"""
        void main() {
           $code
        }
      """

      state.init(Seq(exeCode))
      state.callTheFunction("main", null, Some(program), false)(state)
  }

  def runGlobalCode(code: String, state: State) = {
    val exeCode = s"""
       $code
    """

    state.init(Seq(exeCode))
  }
}

case class ErrorLocation(file: File, line: Int, column: Int)

case class BuildError(problemPath: Seq[ErrorLocation], function: Option[String], errorType: String, error: String)

class SyntaxLogger extends Logger {
  
  val errors = new ListBuffer[String]()

  def addErrors(newErrors: Seq[String]) = {
    errors ++= newErrors
  }
  
  def in(stream: OutputStream) = {}
  def out(stream: InputStream) = {}
  def err(stream: InputStream) = {

    val lines = scala.io.Source.fromInputStream(stream).getLines.toSeq
    println(lines)
    val errors = getErrors(lines)
  }
}

class RunLogger {
  
  def process = new ProcessIO(in, out, err)

  val stdout = new ListBuffer[String]()
  
  def recordStdOut(lines: Seq[String]) = {
    stdout ++= lines
  }
  
  def in(stream: OutputStream) = {}
  def out(stream: InputStream) = {
    recordStdOut(scala.io.Source.fromInputStream(stream).getLines.toSeq)
  }
  def err(stream: InputStream) = {
    val lines = scala.io.Source.fromInputStream(stream).getLines.toSeq
  }
}

class LinkerLogger extends Logger {
  
  val errors = new ListBuffer[String]()

  def addErrors(newErrors: Seq[String]) = {
    errors ++= newErrors
  }

  def in(stream: OutputStream) = {}
  def out(stream: InputStream) = { scala.io.Source.fromInputStream(stream).getLines }
  def err(stream: InputStream) = {

    val lines = scala.io.Source.fromInputStream(stream).getLines.toSeq

    if (!lines.isEmpty) {
      lines.foreach(x => println("LINKER ERROR: " + x))
    }
  }
}

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

  def addErrors(errors: Seq[String])
  
  def getErrors(lines: Seq[String]): Seq[String] = {
    lines
  }
}