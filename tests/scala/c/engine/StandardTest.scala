package c.engine

import org.scalatest._
import better.files._

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import ExecutionContext.Implicits.global

class StandardTest extends FlatSpec {

  def getResults(stdout: List[Char]): List[String] = {
    if (!stdout.isEmpty) {
      stdout.mkString.split("\\n").toList ++ (if (stdout.reverse.take(2) == List('\n', '\n')) List("") else List())
    } else {
      List()
    }
  }

  def checkResults(code: String, shouldBootstrap: Boolean = true): Unit = checkResults2(Seq(code), shouldBootstrap)

  def checkResults2(codeInFiles: Seq[String], shouldBootstrap: Boolean = true) = {

    var except: Exception = null

    val gccOutputFuture = Future[Seq[String]] {
      var result: Seq[String] = Seq()
      try {
        result = Gcc.compileAndGetOutput(codeInFiles)
      } catch {
        case e: Exception => except = e
      }
      result
    }

    val cEngineOutputFuture = Future[List[String]] {

      var result: List[String] = List()
      try {
        val start = System.nanoTime
        val state = new State
        if (shouldBootstrap) {
          state.init(codeInFiles, state)
        } else {
          state.init(Seq("#define HAS_FLOAT\n" + File("src\\scala\\c\\engine\\ee_printf.c").contentAsString) ++ codeInFiles.map { code => "#define printf ee_printf \n" + code }, state)
        }

        state.context.pathStack.push(NodePath(state.getFunction("main").node, Stage1))
        state.context.run
        //totalTime += (System.nanoTime - start) / 1000000000.0
        result = getResults(state.stdout.toList)

      } catch {
        case e: Exception => except = e
      }
      result
    }

    val testExe = for {
      gcc <- gccOutputFuture
      cEngine <- cEngineOutputFuture
    } yield (gcc, cEngine)

    val result = Await.ready(testExe, Duration.Inf).value.get

    result match {
      case Success((gccOutput, cEngineOutput)) =>
        info("C_Engine output: " + cEngineOutput)
        info("Gcc output: " + gccOutput)

        if (except != null) {
          throw except
        }

        assert(cEngineOutput == gccOutput.toList)
      case Failure(e) =>
        e.printStackTrace()
        false
    }
  }
}