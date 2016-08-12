package scala.astViewer

import org.scalatest._

class printf extends FlatSpec with ShouldMatchers {
    "A simple print with inline string" should "print the correct results" in {
      val code = """
        void main() {
          printf("Hello World!\n");
        }"""

      val executor = new Executor(code)
      executor.execute
      executor.stdout.headOption should equal (Some("Hello World!"))
    }

  "A simple print with integer" should "print the correct results" in {
    val code = """
        void main() {
          printf("%d\n", 1);
        }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("1"))
  }

  "A simple print with a single argument string" should "print the correct results" in {
    val code = """
      void main() {
        printf("%s\n", "Hello World!");
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("Hello World!"))
  }

  "A simple print with two arguments" should "print the correct results" in {
    val code = """
      void main() {
        printf("%s %s\n", "Hello", "World!");
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("Hello World!"))
  }
}
