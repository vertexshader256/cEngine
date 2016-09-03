package scala

import org.scalatest._
import app.astViewer._

package object astViewer {
  class StandardTest extends FlatSpec with ShouldMatchers with ParallelTestExecution {
    def checkResults(code: String) = {
      val gccOutput = Gcc.compileAndGetOutput(code)
    
      val executor = new Executor(code)
      executor.execute
      val cEngineOutput = executor.mainContext.stdout
      info("C_Engine output: " + cEngineOutput)
      info("Gcc output: " + gccOutput)
      executor.mainContext.stdout should equal (gccOutput)
    }
  }
}