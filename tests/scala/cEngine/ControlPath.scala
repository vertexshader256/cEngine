package cEngine

class SwitchStatement extends StandardTest {
  "A switch statement" should "print the correct results" in {
    val code = """
      void main() {
        int x = 2;
        switch(x) {
          case 3:
            printf("1\n");
            break;
          case 2:
            printf("3\n");
            break;
          default :
            printf("2\n");
            break;
        }
         
        x = 3;
        switch(x) {
          case 3:
            printf("1\n");
            break;
          case 2:
            printf("3\n");
            break;
          default :
            printf("2\n");
            break;
        }
         
      }"""

    checkResults(code)
  }
  
  "A switch statement with a pointer to char" should "print the correct results" in {
    val code = """
      void main() {
        char x = 'a';
        char *ptr = &x;
        switch(*ptr) {
          case 'a':
            printf("1\n");
            break;
          case 'b':
            printf("3\n");
            break;
          default :
            printf("2\n");
            break;
        }
         
      }"""

    checkResults(code)
  }
  
  "A switch statement with fallthrough" should "print the correct results" in {
    val code = """
      void main() {
        int x = 3;
        switch(x) {
          case 3:
            printf("1\n");
          case 2:
            printf("3\n");
          default :
            printf("2\n");
            break;
        }
      }"""

    checkResults(code)
  }
  
  "A switch statement hitting default value" should "print the correct results" in {
    val code = """
      void main() {
        int x = 300;
        switch(x) {
          case 3:
            printf("1\n");
            break;
          case 2:
            printf("3\n");
            break;
          default :
            printf("2\n");
            break;
        }
      }"""

    checkResults(code)
  }
  
  "A nested switch statement" should "print the correct results" in {
    val code = """
      void main() {
         int a = 100;
         int b = 200;
       
         switch(a) {
            case 100: 
               printf("1\n");
               switch(b) {
                  case 200:
                     printf("2\n");
               }
            case 300:
              printf("3\n");
         }         
      }"""

    checkResults(code)
  }
  
  "A nested switch statement 2" should "print the correct results" in {
    val code = """
      void main() {
         int a = 300;
         int b = 200;
       
         switch(a) {
            case 100: 
               printf("1\n");
               switch(b) {
                  case 200:
                     printf("2\n");
               }
            case 300:
              printf("3\n");
         }         
      }"""

    checkResults(code)
  }
}

class DoWhileStatement extends StandardTest {
  "A simple do..while statement" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        do {
          x++;
        } while (x < 10);
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "An immediately exited do..while statement" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        do {
          x++;
        } while (x == -1);
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
}

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
  
  "A simple while statement reading from struct" should "print the correct results" in {
    val code = """
      
      struct Test {
        int value;
      };
      
      void main() {
        struct Test x = {10};
        while (x.value) {
          x.value--;
          printf("%d\n", x.value);
        }
      }"""

    checkResults(code)
  }
  
  "robust while loop test" should "print the correct results" in {
      val code = """
       void main() {
        int x = 0;
        int a = 0;
        char b = 0;
        float c = 0.0f;
        double d = 0.0;
        long e = 0;
        short f = 0;
        """ + List('a', 'b', 'e', 'f').map{ x => s"""
            x = 0;
            while ($x < 10) {
              $x = $x + 1;
              x++;
            }
            
            printf("%d\\n", x);"""
          }.reduce(_ + "\n" + _) ++ List('c', 'd').map{ x => s"""
            x = 0;
            while ($x < 10.0) {
              $x = $x + 1.0;
              x++;
            }
            
            printf("%d\\n", x);"""
          }.reduce(_ + "\n" + _) + "}"
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

class TernaryTest extends StandardTest {
  "A simple ternary evaluat" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5 == 5 ? 3 : 1;
        int y = 5 > 6 ? 3 : 1;
        printf("%d %d\n", x, y);
      }"""

    checkResults(code)
  }

}

class GotoStatement extends StandardTest {
  "A simple goto statement" should "print the correct results" in {
    val code =
      """
      void main() {
        int x = 0;
        printf("1\n");
        printf("2\n");
        whoa:
        x++;
        printf("%d\n", x);
        printf("4\n");
        if (x < 4) {
           goto whoa;
        }
      }
      """

    checkResults(code)
  }
}

class IfStatement extends StandardTest {
  "A simple if statement with true literal" should "print the correct results" in {
    val code = """
      void main() {
        if (1) {
          printf("1\n");
        } else {
          printf("2\n");
        }
      }"""

    checkResults(code)
  }
  
  "A simple false if statement without an ELSE statement" should "print the correct results" in {
    val code = """
      void main() {
        if (0) {
          printf("1\n");
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
          printf("1\n");
        } else {
          printf("2\n");
        }
      }"""

    checkResults(code)
  }

  "A simple if statement with false variable" should "print the correct results" in {
    val code = """
      void main() {
        int x = 1;
        if (x) {
          printf("1\n");
        } else {
          printf("2\n");
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
          printf("1\n");
        } else {
          printf("2\n");
        }
      }"""

    checkResults(code)

    val code2 = """
      void main() {
        int x = 2 == 2;
        if (x) {
          printf("1\n");
        } else {
          printf("2\n");
        }
      }"""

    checkResults(code2)
  }

  "A simple if statement with false binary comparison" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        if (x == 5) {
          printf("1\n");
        } else {
          printf("2\n");
        }
      }"""

    checkResults(code)

    val code2 = """
      void main() {
        int x = 4;
        if (x == 5) {
          printf("1\n");
        } else {
          printf("2\n");
        }
      }"""

    checkResults(code2)
  }

  "simple nested if statements" should "print the correct results" in {
    val code = """
      void main() {
        if (1) {
          if (0) {
            printf("1\n");
          } else {
            printf("2\n");
          }
        } else {
          printf("3\n");
        }
      }"""

    checkResults(code)
  }
}