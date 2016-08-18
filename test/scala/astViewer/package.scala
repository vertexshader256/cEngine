package scala

import org.scalatest._
import scala.astViewer._

package object astViewer {
  class StandardTest extends FlatSpec with ShouldMatchers with ParallelTestExecution {
    def checkResults(code: String) = {
      val gccOutput = Gcc.compileAndGetOutput(code)
    
      val executor = new Executor(code)
      executor.execute
      executor.mainContext.stdout.headOption should equal (Some(gccOutput.head))
    }
  }
}