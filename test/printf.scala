package scala.astViewer

import org.scalatest._

class printf extends FlatSpec with ShouldMatchers {
    "A simple if statement" should "print the correct results" in {
      val code = """
        void main() {
          printf("Hello World!\n");
        }"""

      val executor = new Executor(code)
      executor.execute
      executor.stdout.headOption should equal (Some("Hello World!"))
    }
}
