package scala.c.engine.gcc

import java.io.{File, PrintWriter}
import scala.c.engine.*
import scala.c.engine.models.*
import scala.c.engine.models.NumBits.*
import scala.sys.process.Process
import scala.util.Try

case class GccOutput(output: Seq[String], wasSuccess: Boolean)

object Gcc {

	def runGlobalCode(code: String, state: CEngine, includePaths: List[String]) = {
		val exeCode = s"$code"
		val ast = Utils.getTranslationUnits(Seq(exeCode), includePaths)
		state.addMain(ast)
	}

	// blocking
	def getGccOutput(cSourceCode: Seq[String], testId: String, pointerSize: NumBits,
														args: List[String], includePaths: List[String]): GccOutput = {

		val logger = new SyntaxLogger
		val exeFile = java.io.File("a" + testId + ".exe")

		val files = cSourceCode.map { code =>
			val file = java.io.File(s"$testId.c")
			val pw = PrintWriter(file)
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

		val builder = Process(processTokens, java.io.File("."))
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
					val path = Seq(exeFile.getAbsolutePath) ++ args
					val runner = Process(path, new File("."))
					val run = runner.run(runLogger.process)

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

			// delete the .c files
			Try(files.foreach(_.delete()))

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