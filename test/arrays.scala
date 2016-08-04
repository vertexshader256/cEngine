package scala.astViewer

import org.scalatest._

class ArrayTest extends FlatSpec with ShouldMatchers {
  "A trivial array assignment" should "print the correct results" in {
    val code = """
      void main() {
        int x[5];
        x[2] = 5;
        printf("%s\n", x[2]);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("5"))
  }
}
