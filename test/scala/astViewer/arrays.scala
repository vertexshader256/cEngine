package scala.astViewer

class ArrayTest extends StandardTest {
  "A trivial array assignment" should "print the correct results" in {
    val code = """
      void main() {
        int x[5];
        x[2] = 5;
        printf("%d\n", x[2]);
      }"""

    checkResults(code)
  }
  
  "A trivial array binary expression" should "print the correct results" in {
    val code = """
      void main() {
        int x[5];
        x[2] = 5;
        x[3] = 3;
        printf("%d\n", x[2] * x[3]);
      }"""

    checkResults(code)
  }
  
  "An array subscript with advanced binary expression" should "print the correct results" in {
    val code = """
      void main() {
        int x[5];
        int y = 2;
        x[1] = 3;
        x[3] = 12;
        printf("%d\n", x[y - 2 + x[1]]);
      }"""

    checkResults(code)
  }
}
