package c.engine

class FloatingPoint extends StandardTest {
  "A simple double initialized global reference" should "print the correct results" in {
    val code = """
      double x = 1.5;
      void main() {
        printf("%.1f\n", x);
      }"""

    checkResults(code)
  }

  "A simple double global reassignment" should "print the correct results" in {
    val code = """
      double x = 1.5;
      void main() {
        x = 3.45;
        printf("%f\n", x);
      }"""

    checkResults(code)
  }

  "A simple uninitialized double global reassignment" should "print the correct results" in {
    val code = """
      double x;
      void main() {
        x = 5.12;
        printf("%f\n", x);
      }"""

    checkResults(code)
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

    checkResults(code)
  }

  "A simple expression with doubles" should "print the correct results" in {
    val code = """
      void main() {
        printf("%f\n", 1.5 * 1.5);
      }"""

    checkResults(code)
  }
}
