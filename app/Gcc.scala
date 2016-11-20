package app.astViewer

import scala.sys.process.ProcessIO
import scala.sys.process._
import better.files._
import scala.collection.mutable.ListBuffer
import java.io.OutputStream
import java.io.InputStream

object Gcc {
  
  val executor = new Executor()
  var isFirst = true
  
  def runCode(code: String, state: State) = {
    
      val exeCode = s"""
        void main() {
           $code
        }
        """
      executor.init(exeCode, isFirst, state)
      isFirst = false
      executor.execute(state)
    }
  
  
  var count = 0
  var count2 = 0
  
  def preprocess(code: String): String = {
    
    var myCount = 0;
    
    synchronized {
      myCount = count2
      count2 += 1
    }
    
    val file = File("tempPreproc" + myCount + ".c")

    file.overwrite(code)
    
    val preprocessTokens = Seq("gcc") ++ Seq("-E", file.path.toString, "-I", "C:\\Scala\\Git\\astViewer\\app")
    val runLogger = new RunLogger
    
    val builder = Process(preprocessTokens, File("").toJava)
    val compile = builder.run(runLogger.process)
    compile.exitValue()
    
    file.delete(true)
    
    runLogger.stdout.reduce{_ + "\n" + _}
  }
  
  def compileAndGetOutput(code: String): Seq[String] = {
    
    val logger = new SyntaxLogger
    val linkerLogger = new LinkerLogger
    val runLogger = new RunLogger
    
    var myCount = 0;
    
    synchronized {
      myCount = count
      count += 1
    }
    
    val file = File("temp" + myCount + ".c")
    val objectFile = File("temp" + myCount + ".o")
    val exeFile = File("temp" + myCount + ".exe")

    file.overwrite(code)
    
    val sourceFileTokens = Seq("-c", file.path.toString)
    val includeTokens = Seq("-I", "C:\\Scala\\Git\\astViewer\\app")

    val processTokens =
        Seq("gcc") ++ sourceFileTokens ++ includeTokens
  
    val builder = Process(processTokens, File("").toJava)
    val compile = builder.run(logger.process)
    compile.exitValue()
    
    val numErrors = logger.errors.filter(_.errorType == "error").length

    val result = if (numErrors == 0) {    
      val linkTokens = Seq("gcc") ++ Seq("-o", exeFile.path.toString, objectFile.path.toString)
      
      val linker = Process(linkTokens, File("").toJava)
      val link = linker.run(linkerLogger.process)
      link.exitValue()
      
      var i = 0
      while (!File(exeFile.path.toString).exists && i < 1000) {
          Thread.sleep(50)
          i += 50
      }
      Thread.sleep(50)

      val runner = Process(Seq(exeFile.path.toString), File("").toJava)
      val run = runner.run(runLogger.process)
      run.exitValue()

      runLogger.stdout
    } else {
      logger.errors.filter(_.errorType == "error").map(_.error)
    }
    
    file.delete(true)
    objectFile.delete(true)
    exeFile.delete(true)
    
    result
  } 
  
  def parseGccErrorString(error: String, function: String, path: Seq[String]): Option[BuildError] = {
    try {
      if ((error contains "error:") ||
        (error contains "warning:") ||
        (error contains "note:") ||
        (error contains "In file included from")) {
        
        if (error.contains("In file included from")) {
          val path = error.split("In file included from ").last
        }
  
        if (error.contains(".c:")) {
          val endOfFilePath = error.indexOf(".c:") + 2
    
          val remaining = error.substring(endOfFilePath + 1)
    
          val tokens = remaining.split(":")
          
          val file = error.substring(0, endOfFilePath)
          val line = Integer.parseInt(tokens(0))
    
          val column = Integer.parseInt(tokens(1))
          val errorType = tokens(2).trim
          Some(BuildError(Seq(ErrorLocation(File(file), line, column)), Some(function), errorType, tokens.splitAt(3)._2.reduce(_ + ":" + _).trim))
        } else if (path.isEmpty) { // immediate header issue
          val endOfFilePath = error.indexOf(".h:") + 2
    
          val remaining = error.substring(endOfFilePath + 1)
    
          val tokens = remaining.split(":")
          
          val file = error.substring(0, endOfFilePath)
          val line = Integer.parseInt(tokens(0))
    
          val column = Integer.parseInt(tokens(1))
          val errorType = tokens(2).trim
          Some(BuildError(Seq(ErrorLocation(File(file), line, column)), Some(function), errorType, tokens.splitAt(3)._2.reduce(_ + ":" + _).trim))
        } else { // nested header issue
          val endOfCFilePath = path.last.indexOf(".c:") + 2
          val fromPos = path.last.indexOf(raw"C:\")
    
          val CfilePath = path.last.substring(fromPos, endOfCFilePath)
          val Cremaining = path.last.substring(endOfCFilePath + 1)
          val Ctokens = Cremaining.split(":")
          val Cline = Integer.parseInt(Ctokens(0))
          
          val endOfFilePath = error.indexOf(".h:") + 2
    
          val remaining = error.substring(endOfFilePath + 1)
    
          val tokens = remaining.split(":")
          val file = error.substring(0, endOfFilePath)
          val line = Integer.parseInt(tokens(0))
    
          val errorLocations = path.reverse.flatMap{ location => 
            if (location contains ".h:") {
              val stripped = location.drop(location.indexOf("C:\\")).reverse.drop(1).reverse
              val endOfFilePath = stripped.indexOf(".h:") + 2
              val remaining = stripped.substring(endOfFilePath + 1)
      
              val tokens = remaining.split(":")
              val file = stripped.substring(0, endOfFilePath)
              val line = Integer.parseInt(tokens(0))
        
              //val column = Integer.parseInt(tokens(1))
              //println("Z: " + ErrorLocation(File(file), line, column))
              Some(ErrorLocation(File(file), line, 0))
            } else {
              None
            }
          }
          
          val column = Integer.parseInt(tokens(1))
          val errorType = tokens(2).trim
          val originatingCFile = File(CfilePath)
          Some(BuildError(Seq(ErrorLocation(originatingCFile, Cline, column)) ++ errorLocations ++ Seq(ErrorLocation(File(file), line, column)), None, errorType, tokens.splitAt(3)._2.reduce(_ + ":" + _).trim))
        }
      } else if (error.contains("In function") && !error.contains("At top level:")) {
        Some(BuildError(Seq(), Some(function), "Misc", error))
      } else {
        None
      }
    } catch {
      case problem: Exception => {
        println("ERROR: " + error)
        println("PATH: " + path)
        problem.printStackTrace()
        None
      }
    }
  }
}

case class ErrorLocation(file: File, line: Int, column: Int)

case class BuildError(problemPath: Seq[ErrorLocation], function: Option[String], errorType: String, error: String)

class SyntaxLogger extends Logger {
  
  val errors = new ListBuffer[BuildError]()

  def addErrors(newErrors: Seq[BuildError]) = {
    errors ++= newErrors
  }
  
  def in(stream: OutputStream) = {}
  def out(stream: InputStream) = {}
  def err(stream: InputStream) = {

    val lines = scala.io.Source.fromInputStream(stream).getLines.toSeq
    val errors = getErrors(lines)

    if (!errors.isEmpty) {

      val fixedFileNameErrors = errors.flatMap { error =>
        error.problemPath.headOption.map { err =>
          error.copy(problemPath = Seq(ErrorLocation(File(err.file.path.toString.replace(".syntax", "")), err.line, err.column)))
        }
      }

      addErrors(fixedFileNameErrors)
    }
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
  
  val errors = new ListBuffer[BuildError]()

  def addErrors(newErrors: Seq[BuildError]) = {
    errors ++= newErrors
  }

 def parseLinkerErrorStrings(error: String, functionName: String): Option[BuildError] = {

    if (error.contains("cannot open output") ||
        error.contains("cannot find") ||
        error.contains("final link failed")) {
        val tokens = error.split("ld.exe:")
        val rawMsg = tokens.last.trim
        val msg = if (rawMsg.contains("Permission denied")) {
          rawMsg + " (Is the app running?)"
        } else {
          rawMsg
        }
        if (msg.startsWith("Warning:")) {
          Some(BuildError(Seq(), Some(functionName), "warning", msg))
        } else {
          Some(BuildError(Seq(), Some(functionName), "linker error", msg))
        }
    } else if (error.contains("(.text")) {
      val tokens = error.split(":")
      val rawMsg = tokens.last.trim
      val fileName = tokens(2)
      Some(BuildError(Seq(ErrorLocation(File(fileName), 0, 0)), Some(functionName), "linker error", rawMsg))
    } else if (error.contains("undefined reference")) {
      val tokens = error.split(":")
      val rawMsg = tokens.last.trim
      val filePath = tokens(0) + ":" + tokens(1)
      Some(BuildError(Seq(ErrorLocation(File(filePath), 0, 0)), Some(functionName), "linker error", rawMsg))
    } else if (error.contains("In function") && !error.contains("At top level:")) {
      if (error.startsWith("Warning:")) {
          Some(BuildError(Seq(), Some(functionName), "warning", error))
        } else {
          Some(BuildError(Seq(), Some(functionName), "linker error", error))
        }
    } else {
      None
    }
  }
  
  private def checkForLinkerErrors(error: String): Option[BuildError] = {
    if (error contains "In function") {
      val start = error.indexOf("`") + 1 // its really weird, the linker emits a backtick, but the compiler emits a single quote "'"
      val end = error.size - 2
      currentFunction = error.substring(start, end)
      None
    } else if (!error.contains("<command-line>")) {
      parseLinkerErrorStrings(error, currentFunction)
    } else {
      None
    }
  }
  
  def getLinkerErrors(lines: Seq[String]): Seq[BuildError] = {
    for {
      errorString <- lines
      error <- checkForLinkerErrors(errorString)
    } yield error
  }
  
  def in(stream: OutputStream) = {}
  def out(stream: InputStream) = { scala.io.Source.fromInputStream(stream).getLines }
  def err(stream: InputStream) = {

    val lines = scala.io.Source.fromInputStream(stream).getLines.toSeq

    if (!lines.isEmpty) {
      lines.foreach(x => println("LINKER ERROR: " + x))
      addErrors(getLinkerErrors(lines.toSeq))
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

  // Advance the state machine
  def checkForErrors(error: String): Option[BuildError] = {
    println(error)
    if (error contains "In function") {
      val start = error.indexOf("\'") + 1
      val end = error.size - 2
      currentFunction = error.substring(start, end)
      None
    } else if (error.contains("In file included from") && error.last == ',') {
      // Some errors span multiple lines, in those cases were should accumulate it
      isParsingSourcePath = true
      errorSource.clear
      errorSource += error
      None
    } else if ((error contains "from ") && error.last == ':') {
      isParsingSourcePath = false
      errorSource += error
      None
    } else if (isParsingSourcePath) {
      errorSource += error
      None
    } else if (!error.contains("<command-line>")) {
      val result = Gcc.parseGccErrorString(error, currentFunction, errorSource)
      result
    } else {
      None
    }
  }

  def addErrors(errors: Seq[BuildError])
  
  def getErrors(lines: Seq[String]): Seq[BuildError] = {
    val result = for {
      errorString <- lines
      error <- checkForErrors(errorString)
    } yield error
    
    // just to be sure everything is unique.  Didnt want to use a set
    result.distinct
  }
}