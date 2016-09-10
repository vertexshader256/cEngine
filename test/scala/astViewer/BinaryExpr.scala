package scala.astViewer

class BinaryExpr extends StandardTest {
  "Order of operations test 3" should "print the correct results" in {
    val code = """
      void main() {
        if ((1 + 2) * (5 - 2) == 9) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
      }"""

    checkResults(code)
  }

  "Two expressions ANDed" should "print the correct results" in {
    val code = """
      void main() {
        if (1 == 1 && 2 == 3) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
        
        if (5 < 10 && 3 == 3) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
      }"""

    checkResults(code)
  }

  "Two expressions ORed" should "print the correct results" in {
    val code = """
      void main() {
      
        // first expr is true
        if (1 == 1 || 2 == 3) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
        
        // second expr is true
        if (1 == 0 || 7 > 3) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
      }"""

    checkResults(code)
  }

  "Function calls as expressions" should "print the correct results" in {
    val code = """
      int test() {
        return 2;
      }
      
      int test2() {
        return 3;
      }

      void main() {
        if (1 == 1 || 2 == test()) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
        
        if (2 == test() || 1 == 0) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
        
        if (test() == test2() - 1) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
      }"""

    checkResults(code)
  }
  
  "A simple increment test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        double y = 5;
        x += 1;
        y += 1.5;
        printf("%d %f\n", x, y);
      }"""

    checkResults(code)
  }
  
  "A simple decrement test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        double y = 5;
        x -= 2;
        y -= 2.0;
        printf("%d %f\n", x, y);
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
  
   
  "A modulus test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 20;
        printf("%d %d %d\n", 10 % 2, x % 10, 10 % x);
      }"""

    checkResults(code)
  }
  
  "A binary OR test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 2147483647;
        printf("%d %d\n", 10 | 2, 1 | x);
      }"""

    checkResults(code)
  }
}
