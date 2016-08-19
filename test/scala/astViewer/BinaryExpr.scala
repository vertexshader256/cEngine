package scala.astViewer

class BinaryExpr extends StandardTest {
  "Order of operations test 3" should "print the correct results" in {
    val code = """
      void main() {
        if ((1 + 2) * (5 - 2) == 9) {
          printf("%s\n", "SUCCESS");
        } else {
          printf("%s\n", "FAIL");
        }
      }"""

    checkResults(code)
  }

  "Two expressions ANDed" should "print the correct results" in {
    val code = """
      void main() {
        if (1 == 1 && 2 == 3) {
          printf("%s\n", "FAIL");
        } else {
          printf("%s\n", "SUCCESS");
        }
      }"""

    checkResults(code)
  }

  "Two expressions ORed" should "print the correct results" in {
    val code = """
      void main() {
        if (1 == 1 || 2 == 3) {
          printf("%s\n", "SUCCESS");
        } else {
          printf("%s\n", "FAIL");
        }
      }"""

    checkResults(code)
    
    val code2 = """
      void main() {
        if (1 == 0 || 7 > 3) {
          printf("%s\n", "FAIL");
        } else {
          printf("%s\n", "SUCCESS");
        }
      }"""

    checkResults(code2)
  }

  "Two expressions ORed with a function call" should "print the correct results" in {
    val code = """
      int test() {
        return 2;
      }

      void main() {
        if (1 == 1 || 2 == test()) {
          printf("%s\n", "SUCCESS");
        } else {
          printf("%s\n", "FAIL");
        }
      }"""

    checkResults(code)
  }
  
  "A simple increment test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        x += 1;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "A simple decrement test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        x -= 2;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "A more complex decrement test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        x -= (x * 4) / 2 + (2 + x) * 2;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
}
