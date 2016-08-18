package scala

import org.scalatest._
import scala.astViewer._

package object astViewer {
  class StandardTest extends FlatSpec with ShouldMatchers {
    def checkResults(code: String) = {
      val gccOutput = Gcc.compileAndGetOutput(code)
    
      val executor = new Executor(code)
      executor.execute
      executor.stdout.headOption should equal (Some(gccOutput.head))
    }
  }
}