import org.scalatest._

import scala.astViewer.Executor

class IfStatement extends FlatSpec with ShouldMatchers {
  "A simple if statement" should "print the correct results" in {
    val code = """
      void main() {
        if (1) {
          printf("%f\n", 1);
        } else {
          printf("%f\n", 2);
        }
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("1"))
  }
}
