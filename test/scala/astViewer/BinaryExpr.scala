package scala.astViewer

import org.scalatest._

class BinaryExpr extends FlatSpec with ShouldMatchers {
  "Order of operations test 3" should "print the correct results" in {
    val code = """
      void main() {
        if ((1 + 2) * (5 - 2) == 9) {
          printf("%s\n", "SUCCESS");
        } else {
          printf("%s\n", "FAIL");
        }
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("SUCCESS"))
  }

  "Two expressions ANDed" should "print the correct results" in {
    val code = """
      void main() {
        if (1 == 1 && 2 == 3) {
          printf("%s\n", "FAIL");
        } else {
          printf("%s\n", "SUCCESS");
        }
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("SUCCESS"))
  }

  "Two expressions ORed" should "print the correct results" in {
    val code = """
      void main() {
        if (1 == 1 || 2 == 3) {
          printf("%s\n", "SUCCESS");
        } else {
          printf("%s\n", "FAIL");
        }
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("SUCCESS"))
  }

  "Two expressions ORed with a function call" should "print the correct results" in {
    val code = """
      int test() {
        return 2;
      }

      void main() {
        if (1 == 1 || 2 == test()) {
          printf("%s\n", "SUCCESS");
        } else {
          printf("%s\n", "FAIL");
        }
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("SUCCESS"))
  }
}
