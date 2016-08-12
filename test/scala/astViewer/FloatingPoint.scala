package scala.astViewer

import org.scalatest._

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

  "A simple double global reassignment" should "print the correct results" in {
    val code = """
      double x = 1.5;
      void main() {
        x = 3.45;
        printf("%f\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("3.45"))
  }

  "A simple uninitialized double global reassignment" should "print the correct results" in {
    val code = """
      double x;
      void main() {
        x = 5.12;
        printf("%f\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("5.12"))
  }

  "A simple function returning a double" should "print the correct results" in {
    val code = """
      double test() {
         return 4.34;
      }
      void main() {
        double x = test();
        printf("%f\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("4.34"))
  }

  "A simple expression with doubles" should "print the correct results" in {
    val code = """
      void main() {
        printf("%f\n", 1.5 * 1.5);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("2.25"))
  }
}
