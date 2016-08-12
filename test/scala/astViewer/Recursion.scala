package scala.astViewer

import org.scalatest._

class Recursion extends FlatSpec with ShouldMatchers {
  "Order of operations test 3" should "print the correct results" in {
    val code = """

      int sums(int x) {
         if (x > 0) {
            return x + sums(x - 1);
         } else {
            return 0;
         }
      }

      void main() {
        printf("%d\n", sums(5));
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("15"))
  }
}
