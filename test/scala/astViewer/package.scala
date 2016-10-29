package scala

import org.scalatest._
import app.astViewer._

package object astViewer {
  class StandardTest extends FlatSpec with ShouldMatchers with ParallelTestExecution {
    def checkResults(code: String) = {
      val gccOutput = Gcc.compileAndGetOutput(code)
    
      val state = new State
      val executor = new Executor(code, state)
      executor.execute
      val cEngineOutput = state.stdout
      info("C_Engine output: " + cEngineOutput)
      info("Gcc output: " + gccOutput)
      state.stdout should equal (gccOutput)
    }
  }
}