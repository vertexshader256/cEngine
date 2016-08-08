import org.scalatest._

import scala.astViewer.Executor

class IfStatement extends FlatSpec with ShouldMatchers {
  "A simple if statement with true literal" should "print the correct results" in {
    val code = """
      void main() {
        if (1) {
          printf("%d\n", 1);
        } else {
          printf("%d\n", 2);
        }
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("1"))
  }

  "A simple if statement with false literal" should "print the correct results" in {
    val code = """
      void main() {
        if (0) {
          printf("%d\n", 1);
        } else {
          printf("%d\n", 2);
        }
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("2"))
  }

  "A simple if statement with false variable" should "print the correct results" in {
    val code = """
      void main() {
        int x = 1;
        if (x) {
          printf("%d\n", 1);
        } else {
          printf("%d\n", 2);
        }
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("1"))
  }

  "A simple if statement with false binary comparison" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        if (x == 5) {
          printf("%d\n", 7);
        } else {
          printf("%d\n", 2);
        }
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("7"))

    val code2 = """
      void main() {
        int x = 4;
        if (x == 5) {
          printf("%d\n", 7);
        } else {
          printf("%d\n", 2);
        }
      }"""

    val executor2 = new Executor(code2)
    executor2.execute
    executor2.stdout.headOption should equal (Some("2"))
  }

  "simple nested if statements" should "print the correct results" in {
    val code = """
      void main() {
        if (1) {
          if (0) {
            printf("%f\n", 1);
          } else {
            printf("%f\n", 3);
          }
        } else {
          printf("%f\n", 2);
        }
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("3"))
  }
}
