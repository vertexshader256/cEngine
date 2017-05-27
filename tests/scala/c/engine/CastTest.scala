package c.engine

class CastTest extends StandardTest {
  "A simple cast" should "print the correct results" in {
    val code = """

      long testLong() {
        return 1543543;
      }

      void main() {
        printf("%f\n", (double)testLong());
      }"""

    checkResults(code)
  }

  "A simple cast from a bin expr" should "print the correct results" in {
    val code = """

      long testLong() {
        return 1543543;
      }

      void main() {
        short x = 5;
        short y = 5;

        printf("%f\n", (double)(82384238328 + 1));
        printf("%f\n", (double)(x + y));
        printf("%d\n", (long)(1.1 + 1.5));
        printf("%d\n", (long)(1.1f + 1.5f));
      }"""

    checkResults(code)
  }

  "A simple cast from a function call" should "print the correct results" in {
    val code = """

      long testLong() {
        return 1543543;
      }

      void main() {
        printf("%f\n", (double)testLong());
      }"""

    checkResults(code)
  }
}