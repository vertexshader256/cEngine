package scala.astViewer

import org.scalatest._

class ForLooptest extends FlatSpec with ShouldMatchers {
  "A simple for loop" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        int i = 0;
        for (i = 0; i < 10; i = i + 1) {
          x = x + 1;
        }
        
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("10"))
  }
  
  "A little more advanced for loop" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        for (int i = 0; i < 10; i = i + 1) {
          x = x + 1;
        }
        
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("10"))
  }
}