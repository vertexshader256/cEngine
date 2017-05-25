package tests.scala

import tests.scala.TestClasses._

class CornerCase extends StandardTest {
  "A strange corner case" should "print the correct results" in {
    val code =
      """
      void main() {
        char c = 3["Hello"];
        printf("%c\n", c);

      }"""

    checkResults(code)
  }
}