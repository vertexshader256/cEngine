package scala.astViewer

class Recursion extends StandardTest {
  "Recursion test 1" should "print the correct results" in {
    val code = """

      int sums(int x) {
         if (x > 0) {
            return x + sums(x - 1);
         } else {
            return 0;
         }
      }

      void main() {
        printf("%d\n", sums(0));
      }"""

    checkResults(code)
    
    val code2 = """

      int sums(int x) {
         if (x > 0) {
            return x + sums(x - 1);
         } else {
            return 0;
         }
      }

      void main() {
        printf("%d\n", sums(1));
      }"""

    checkResults(code2)
    
    val code3 = """

      int sums(int x) {
         if (x > 0) {
            return x + sums(x - 1);
         } else {
            return 0;
         }
      }

      void main() {
        printf("%d\n", sums(5));
      }"""

    checkResults(code3)
  }
}
