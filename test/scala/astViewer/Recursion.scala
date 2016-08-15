package scala.astViewer

import org.scalatest._

class Recursion extends FlatSpec with ShouldMatchers {
  "Recursion test 1" should "print the correct results" in {
    val code = """

      int sums(int x) {
         if (x > 0) {
            return x + sums(x - 1);
         } else {
            return 0;
         }
      }

      void main() {
        printf("%d\n", sums(0));
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("0"))
  }
  
  "Recursion test 2" should "print the correct results" in {
    val code = """

      int sums(int x) {
         if (x > 0) {
            return x + sums(x - 1);
         } else {
            return 0;
         }
      }

      void main() {
        printf("%d\n", sums(1));
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("1"))
  }
  
  "Recursion test 3" should "print the correct results" in {
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
