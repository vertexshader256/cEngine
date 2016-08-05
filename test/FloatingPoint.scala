import org.scalatest._

import scala.astViewer.Executor

class FloatingPoint extends FlatSpec with ShouldMatchers {
  "A simple double initialized global reference" should "print the correct results" in {
    val code = """
      double x = 1.5;
      void main() {
        printf("%f\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("1.5"))
  }
}
