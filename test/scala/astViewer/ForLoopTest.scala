package scala.astViewer

class ForLooptest extends StandardTest {
  "A simple for loop" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        int i = 0;
        for (i = 0; i < 10; i = i + 1) {
          x = x + 1;
        }
        
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "A simple for loop with an integer to evaluate" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        int i = 10;
        for (i = 0; i; i--) {
          printf("%d\n", i);
        }
      }"""

    checkResults(code)
  }
  
  "A for loop with multiple post expr" should "print the correct results" in {
    val code = """
      void main() {
        int i = 0;
        int j = 0;
        int n = 7;
        for (i = 0, j = n - 1; i < 5; i++, j--) {
          printf("%d %d\n", i, j);
        }
      }"""

    checkResults(code)
  }
  
  "A for loop without an expression" should "print the correct results" in {
    val code = """
      void main() {
        int j = 0;
        int max = 24;
        for (j = 0; j < max; j++);
        printf("%d\n", j);
      }"""

    checkResults(code)
  }
  
  "A for loop without a stopping point" should "print the correct results" in {
    val code = """
      void main() {
        int i = 0;
        int j = 0;
        int n = 7;
        for (i = 0, j = n - 1;; i++, j--) {
          printf("%d %d\n", i, j);
          if (i == 5)
            break;
        }
      }"""

    checkResults(code)
  }
  
  "A little more advanced for loop" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        int i = 0;
        for (i = 0; i < 10; i = i + 1) {
          x = x + 1;
        }
        
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "A for loop with a continue" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        int i = 0;
        for (i = 0; i < 10; i = i + 1) {
          if (i == 3 || i == 5 || i == 7) {
            continue;
          }
          printf("looping\n");
          x = x + 1;
        }
        
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "A for loop with a break" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        int i = 0;
        for (i = 0; i < 10; i = i + 1) {
          x = x + 1;
          printf("looping\n");
          if (i > 2) {
            break;
          }
        }
        
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "A for loop with a function call" should "print the correct results" in {
    val code = """
      
      int blah = 0;
      
      void increment() {
        blah++;
      }
      
      void main() {
        int i = 0;
        for (i = 0; i < 10; i += 2) {
          increment();
        }
        
        printf("%d\n", blah);
      }"""

    checkResults(code)
  }
  
  "A for loop setting an array" should "print the correct results" in {
    val code = """

      void main() {
        int x[5];
        int i = 0;
        for (i = 0; i < 5; i++) {
          x[i] = i;
        }
        
        printf("%d\n", x[3]);
      }"""

    checkResults(code)
  }
}