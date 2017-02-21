package scala

import org.scalatest._
import app.astViewer._
import scala.concurrent._

import ExecutionContext.Implicits.global
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.util.Success
import scala.util.Failure
import better.files._

package object astViewer {
  
  var totalTime: Double = 0.0
  
  class StandardTest extends FlatSpec with ShouldMatchers with ParallelTestExecution {
    
    def checkResults(code: String): Unit = checkResults(Seq(code))
    
    def checkResults(codeInFiles: Seq[String]) = {

      //val codeWithStdio = File("app\\ee_printf.c").contentAsString + code
      
      val gccOutputFuture = Future[Seq[String]] { Gcc.compileAndGetOutput(codeInFiles) }
    
      val cEngineOutputFuture = Future[ListBuffer[String]] {
        val start = System.nanoTime
        val state = Executor.init(codeInFiles, true)
        Executor.run(state)
        totalTime += (System.nanoTime - start)/1000000000.0
        state.stdout
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
          cEngineOutput should equal (gccOutput)
        case Failure(e) => 
          e.printStackTrace()
          e.getMessage should equal (false)
      }
    }
  }
}