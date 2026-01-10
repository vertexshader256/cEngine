package scala.c.engine

import java.io.{File, InputStream, OutputStream, PrintWriter}
import scala.c.engine.models.NumBits.*
import scala.collection.mutable.ListBuffer
import scala.sys.process.ProcessIO
import scala.sys.process.Process
import scala.c.engine.models.*

case class GccOutput(output: Seq[String], wasSuccess: Boolean)

object Gcc {

	val program = new FunctionScope(List(), null, null) {}

	def runCode(code: String, state: State, includePaths: Iterator[String]) = {
		val exeCode =
			s"""
        void main() {
           $code
        }
      """

		val ast = State.parseCode(Seq(exeCode), includePaths.toList)
		state.addMain(ast)
		state.callTheFunction("main", null, Some(program), true)(using state)

		val main = state.functionList.find(_.name == "main").get
		state.functionList -= main
	}

	def runGlobalCode(code: String, state: State, includePaths: List[String]) = {
		val exeCode =
			s"""
       $code
    """

		val ast = State.parseCode(Seq(exeCode), includePaths)
		state.addMain(ast)
	}

	// blocking
	def getGccOutput(cSourceCode: Seq[String], testId: String, pointerSize: NumBits = ThirtyTwoBits,
														args: List[String] = List(), includePaths: List[String] = List()): GccOutput = {

		val logger = new SyntaxLogger
		val exeFile = new java.io.File("a" + testId + ".exe")

		val files = cSourceCode.map { code =>
			val file = new java.io.File(s"$testId.c")
			val pw = new PrintWriter(file)
			pw.write("#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n" + code)
			pw.close()
			file
		}

		val moreIncludes = includePaths.flatMap { inc =>
			Seq("-I", inc)
		}

		val sourceFileTokens = files.flatMap { file => Seq(file.getAbsolutePath) }
		val includeTokens = Seq("-I", Utils.mainPath) ++ moreIncludes

		val size = pointerSize match {
			case ThirtyTwoBits => Seq("gcc")
			case SixtyFourBits => Seq("gcc")
		}

		val processTokens =
			size ++ sourceFileTokens ++ includeTokens ++ Seq("-o", exeFile.getAbsolutePath) ++ Seq("-D", "ALLOC_TESTING")

		val builder = Process(processTokens, new java.io.File("."))
		val compile = builder.run(logger.process)

		compile.exitValue()

		logger.errors.toList.foreach(println)

		val numErrors = 0 //logger.errors.length

		val gccOutput = if (numErrors == 0) {

			var isDone = false
			val maxTries = 50 // 50 is proven to work
			var i = 0
			var result: Seq[String] = null

			Thread.sleep(30)

			// 3/1/19: Protip - This helps tests run reliably!
			while (!isDone && i < maxTries) {

				i += 1
				try {
					val runLogger = new RunLogger
					// run the actual executable
					val runner = Process(Seq(exeFile.getAbsolutePath) ++ args, new File("."))
					val run = runner.run(runLogger.process)

					files.foreach(_.delete())

					run.exitValue()

					result = runLogger.stdout.clone().toList

					if (result.nonEmpty) {
						isDone = true
						exeFile.delete()
					}
				} catch {
					case e: Throwable => Thread.sleep(50)
				}
			}

			GccOutput(result, true)
		} else {
			GccOutput(logger.errors.toSeq, false)
		}

		if gccOutput != null then {

			if (gccOutput.output != null) {
				val hasNoCompileError = !gccOutput.output.exists(x => x.contains("returned 1 exit status"))

				if hasNoCompileError then
					gccOutput
				else
					GccOutput(gccOutput.output, false)
			} else {
				GccOutput(Seq("Compilation error"), false)
			}
		} else
			GccOutput(logger.errors.toSeq, false)
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

		val lines = scala.io.Source.fromInputStream(stream).getLines().toList
		errors ++= lines
	}
}

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