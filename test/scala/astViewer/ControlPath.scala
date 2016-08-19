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
  
  "A simple false if statement without an ELSE statement" should "print the correct results" in {
    val code = """
      void main() {
        if (0) {
          printf("%d\n", 1);
        }
      }"""

    checkResults(code)
  }
  
  "A BREAK within a nested IF statement within a WHILE" should "print the correct results" in {
    val code = """
      void main() {
      int x = 0;
        while (1) {
          if (x > 5) {
            break;
          }
          x++;
          printf("LOOPING\n");
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
  
  "A simple if statement with many ELSE IF statements" should "print the correct results" in {
    val code = """
      void main() {
        int x;
        for (x = 0; x < 8; x++) {
          if (x == 0) {
            printf("path1\n");
          } else if (x == 1) {
            printf("path2\n");
          } else if (x == 2) {
            printf("path3\n");
          } else if (x == 3) {
            printf("path4\n");
          } else if (x == 4) {
            printf("path5\n");
          } else if (x == 5) {
            printf("path6\n");
          } else if (x == 6) {
            printf("path7\n");
          } else {
            printf("path8\n");
          }
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
