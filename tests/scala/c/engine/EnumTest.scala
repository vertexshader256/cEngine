package scala.c.engine

class EnumTest extends StandardTest {

  "A simple enum" should "print the correct results" in {
    val code =
      """

      enum { TEST = 1 };

      void main() {
        printf("%d\n", TEST);
      }"""

    checkResults(code)
  }
}