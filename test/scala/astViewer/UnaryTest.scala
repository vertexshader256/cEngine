package scala.astViewer

import org.scalatest._

class UnaryTest extends FlatSpec with ShouldMatchers {
  "A simple postfix increment test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        x++;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("1"))
  }
  
  
  "A simple postfix decrement test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        x--;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("4"))
  }
}
