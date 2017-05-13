package tests.scala

import tests.scala.TestClasses._

class SignTest extends StandardTest {
  "unsigned test 1" should "print the correct results" in {
    val code = """
      void main() {
        unsigned int x = 2147483647;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "unsigned test involving negatives" should "print the correct results" in {
    val code = """
      void main() {
        unsigned int x = -10;
        unsigned int y = 10 + x;
        printf("%d\n", y);
      }"""

    checkResults(code)
  }
}
