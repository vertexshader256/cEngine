package scala.astViewer

class printf extends StandardTest {
    "A simple print with inline string" should "print the correct results" in {
      val code = """
        void main() {
          printf("Hello World!\n");
        }"""

      checkResults(code)
    }

  "A simple print with integer" should "print the correct results" in {
    val code = """
        void main() {
          printf("%d\n", 1);
        }"""

    checkResults(code)
  }

  "A simple print with a single argument string" should "print the correct results" in {
    val code = """
      void main() {
        printf("%s\n", "Hello World!");
      }"""

    checkResults(code)
  }

  "A simple print with two arguments" should "print the correct results" in {
    val code = """
      void main() {
        printf("%s %s\n", "Hello", "World!");
      }"""

    checkResults(code)
  }
}
