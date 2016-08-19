package scala.astViewer

class WhileStatement extends StandardTest {
  "A simple while statement" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        while (x < 10) {
          x++;
        }
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "A simple while statement with a break" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        while (1) {
          break;
          x++;
        }
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "Nested while statements with break" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        while (1) {
          while (1) {
            break;
            x++;
          }
          x++;
          break;
        }
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
//  "A simple while statement with a break" should "print the correct results" in {
//    val code = """
//      void main() {
//        int x = 0;
//        while (1) {
//          if (x > 5) {
//            break;
//          }
//          x++;
//        }
//        printf("%d\n", x);
//      }"""
//
//    checkResults(code)
//  }
}

class IfStatement extends StandardTest {
  "A simple if statement with true literal" should "print the correct results" in {
    val code = """
      void main() {
        if (1) {
          printf("%d\n", 1);
        } else {
          printf("%d\n", 2);
        }
      }"""

    checkResults(code)
  }

  "A simple if statement with false literal" should "print the correct results" in {
    val code = """
      void main() {
        if (0) {
          printf("%d\n", 1);
        } else {
          printf("%d\n", 2);
        }
      }"""

    checkResults(code)
  }

  "A simple if statement with false variable" should "print the correct results" in {
    val code = """
      void main() {
        int x = 1;
        if (x) {
          printf("%d\n", 1);
        } else {
          printf("%d\n", 2);
        }
      }"""

    checkResults(code)
  }

  "A simple if statement with boolean variable" should "print the correct results" in {
    val code = """
      void main() {
        int x = 1 == 2;
        if (x) {
          printf("%d\n", 1);
        } else {
          printf("%d\n", 2);
        }
      }"""

    checkResults(code)

    val code2 = """
      void main() {
        int x = 2 == 2;
        if (x) {
          printf("%d\n", 1);
        } else {
          printf("%d\n", 2);
        }
      }"""

    checkResults(code2)
  }

  "A simple if statement with false binary comparison" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        if (x == 5) {
          printf("%d\n", 7);
        } else {
          printf("%d\n", 2);
        }
      }"""

    checkResults(code)

    val code2 = """
      void main() {
        int x = 4;
        if (x == 5) {
          printf("%d\n", 7);
        } else {
          printf("%d\n", 2);
        }
      }"""

    checkResults(code2)
  }

  "simple nested if statements" should "print the correct results" in {
    val code = """
      void main() {
        if (1) {
          if (0) {
            printf("%d\n", 1);
          } else {
            printf("%d\n", 3);
          }
        } else {
          printf("%d\n", 2);
        }
      }"""

    checkResults(code)
  }
}
