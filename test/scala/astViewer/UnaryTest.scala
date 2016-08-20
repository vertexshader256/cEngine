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
  
  "A a negative number and variable" should "print the correct results" in {
    val code = """
      
      int func() {
        return 5;
      }
      
      double func2() {
        return 5.5;
      }
      
      void main() {
        int a = 5;
        double x = 4.2;
        printf("%d %d %d %f %f\n", -6, -a, -func(), -func2(), -x);
      }"""

    checkResults(code)
  }
}
