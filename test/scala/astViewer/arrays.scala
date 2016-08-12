package scala.astViewer

import org.scalatest._

class ArrayTest extends FlatSpec with ShouldMatchers {
  "A trivial array assignment" should "print the correct results" in {
    val code = """
      void main() {
        int x[5];
        x[2] = 5;
        printf("%d\n", x[2]);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("5"))
  }
  
  "A trivial array binary expression" should "print the correct results" in {
    val code = """
      void main() {
        int x[5];
        x[2] = 5;
        x[3] = 3;
        printf("%d\n", x[2] * x[3]);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("15"))
  }
}
