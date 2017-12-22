package scala.c.engine

import scala.sys.process.ProcessIO
import scala.sys.process._
import java.io.File

import scala.collection.mutable.ListBuffer
import java.io.{InputStream, OutputStream, PrintWriter}

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
      state.callTheFunction("main", null, Array(), Some(program))

      println(state.context.varMap)

      val main = state.getFunction("main")
      state.functionList -= main
  }

  def runGlobalCode(code: String, state: State) = {
    val exeCode = s"""
       $code
    """

    state.init(Seq(exeCode))
  }
  
  
  var count = 0
  var count2 = 0
  
  def compileAndGetOutput(codes: Seq[String]): Seq[String] = {
    
    val logger = new SyntaxLogger
    val linkerLogger = new LinkerLogger
    val runLogger = new RunLogger

    val files = codes.map{ code =>
      val file = File.createTempFile("cEng", ".c", new File("."))
      val pw = new PrintWriter(file)
      pw.write(code)
      pw.close
      file
    }
    
    val objectFiles = files.map{ file =>
      new File(file.getAbsolutePath.reverse.drop(2).reverse + ".o")
    } 
    
    val exeFile = File.createTempFile("cEng", ".exe", new File("."))
    
    val sourceFileTokens = files.flatMap{file => Seq("-c", file.getAbsolutePath)}
    val includeTokens = Seq("-I", Utils.mainPath, 
                            "-I", Utils.mainAdditionalPath)

    val processTokens =
        Seq("gcc") ++ sourceFileTokens ++ includeTokens ++ Seq("-D", "ALLOC_TESTING")
  
    val builder = Process(processTokens, new File("."))
    val compile = builder.run(logger.process)
    compile.exitValue()
    
    val numErrors = logger.errors.length

    val result = if (numErrors == 0) {    
      val linkTokens = Seq("gcc") ++ Seq("-o", exeFile.getAbsolutePath) ++ objectFiles.map(_.getAbsolutePath)
      
      val linker = Process(linkTokens, new File("."))
      val link = linker.run(linkerLogger.process)
      link.exitValue()
      
      var i = 0
      while (!new File(exeFile.getAbsolutePath).exists && i < 1000) {
          Thread.sleep(50)
          i += 50
      }
      Thread.sleep(50)

      val runner = Process(Seq(exeFile.getAbsolutePath), new File("."))
      val run = runner.run(runLogger.process)
      run.exitValue()

      runLogger.stdout
    } else {
      logger.errors
    }
    
    files.foreach{file => file.delete()}
    objectFiles.foreach{file => file.delete()}
    exeFile.delete()
    
    result
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