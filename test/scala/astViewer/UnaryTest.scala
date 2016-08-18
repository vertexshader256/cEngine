package scala.astViewer

class UnaryTest extends StandardTest {
  "A simple postfix increment test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        x++;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  
  "A simple postfix decrement test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        x--;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "A simple prefix init test" should "print the correct results" in {
    val code = """
      void main() {
        int a = 0;
        int b = ++a;
        printf("%d %d\n", a, b);
      }"""

    checkResults(code)
  }
  
  "A simple postfix init test" should "print the correct results" in {
    val code = """
      void main() {
        int a = 0;
        int b = a++;
        printf("%d %d\n", a, b);
      }"""

    checkResults(code)
  }
}
