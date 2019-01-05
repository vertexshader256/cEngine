package scala.c.engine

import java.io.{File, PrintWriter}
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest._
import scala.concurrent._
import scala.collection.mutable.ListBuffer
import scala.sys.process.Process
import scala.util.Try

object StandardTest {
  val cFileCount = new AtomicInteger()
  val exeCount = new AtomicInteger()
}

class StandardTest extends AsyncFlatSpec with ParallelTestExecution {

  implicit override def executionContext = scala.concurrent.ExecutionContext.Implicits.global

  def getResults(stdout: List[Char]): List[String] = {
    if (!stdout.isEmpty) {
      val results = new ListBuffer[String]()

      var currentString = new ListBuffer[Char]()
      var writeLast = false

      var index = 0
      while (index < stdout.size) {

        if (stdout(index) == '\r') {
          results += currentString.mkString
          currentString = new ListBuffer[Char]()
          writeLast = false
          index += 1
        }  else if (stdout(index) == '\n') {
          results += currentString.mkString
          currentString = new ListBuffer[Char]()
          writeLast = false
          index += 1
        } else {
          currentString += stdout(index)
          writeLast = true
          index += 1
        }

      }

      if (writeLast) {
        results += currentString.mkString
      }
      results.toList
    } else {
      List()
    }
  }

  def checkResults(code: String, shouldBootstrap: Boolean = true, pointerSize: NumBits = ThirtyTwoBits) = checkResults2(Seq(code), shouldBootstrap, pointerSize)

  def getCEngineResults(codeInFiles: Seq[String], shouldBootstrap: Boolean, pointerSize: NumBits) = {
    Try {
      //val start = System.nanoTime
      val state = new State(pointerSize)
      val node = if (shouldBootstrap) {
        state.init(codeInFiles)
      } else {
        state.init(Seq("#define HAS_FLOAT\n" + better.files.File("./src/scala/c/engine/ee_printf.c").contentAsString) ++ codeInFiles.map { code => "#define printf ee_printf \n" + code })
      }

      val program = new FunctionScope(List(), null, null) {}
      state.pushScope(program)
      program.init(node, state, false)

      state.context.run(state) // parse globals

      state.context.setAddress(0)

      //state.context.pathStack.push(NodePath(state.getFunction("main").node, Stage1))
      state.callTheFunction("main", null, None)(state)
      //totalTime += (System.nanoTime - start) / 1000000000.0
      getResults(state.stdout.toList)
    }.getOrElse(List())
  }

  def checkResults2(codeInFiles: Seq[String], shouldBootstrap: Boolean = true, pointerSize: NumBits = ThirtyTwoBits) = {

    var except: Exception = null

    Future {

      var cEngineOutput: List[String] = List()
      var gccOutput = Seq[String]()

      try {

        val logger = new SyntaxLogger
        val runLogger = new RunLogger

        val files = codeInFiles.map{ code =>
          val file = new java.io.File(StandardTest.cFileCount.incrementAndGet + ".c")
          val pw = new PrintWriter(file)
          pw.write(code)
          pw.close
          file
        }

        val exeFile = new java.io.File(StandardTest.exeCount.incrementAndGet + ".exe")
        val sourceFileTokens = files.flatMap{file => Seq(file.getAbsolutePath)}
        val includeTokens = Seq("-I", Utils.mainPath,
          "-I", Utils.mainAdditionalPath)

        val size = pointerSize match {
          case ThirtyTwoBits => Seq("gcc")
          case SixtyFourBits => Seq("gcc64")
        }

        val processTokens =
          size ++ sourceFileTokens ++ includeTokens ++ Seq("-o", exeFile.getAbsolutePath) ++ Seq("-D", "ALLOC_TESTING")

        val builder = Process(processTokens, new java.io.File("."))
        val compile = builder.run(logger.process)

        // while its compiling, run the cEngine code

        cEngineOutput = getCEngineResults(codeInFiles, shouldBootstrap, pointerSize)

        compile.exitValue()

        val numErrors = logger.errors.length

        gccOutput = if (numErrors == 0) {

          var isDone = false

          while (!isDone) {
            try {
              // run the actual executable
              val runner = Process(Seq(exeFile.getAbsolutePath), new File("."))
              val run = runner.run(runLogger.process)

              // delete files while program is running
              files.foreach{file => file.delete()}

              run.exitValue()
              isDone = true
            } catch {
              case e =>
            }
          }

          runLogger.stdout
        } else {
          logger.errors
        }

        Future {exeFile.delete()}

      } catch {
        case e: Exception => except = e
      }

      info("C_Engine output: " + cEngineOutput)
      info("Gcc      output: " + gccOutput.toList)

      if (except != null) {
        throw except
      }

      assert(cEngineOutput.map{_.getBytes.toList} == gccOutput.toList.map{_.getBytes.toList})
    }
  }
}