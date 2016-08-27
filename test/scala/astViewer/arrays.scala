package scala.astViewer

class ArrayInitTest extends StandardTest {
  "A array assignment with an init list of ints" should "print the correct results" in {
    val code = """
      void main() {
        int x[5] = {1, 2, 3, 4, 5};
        printf("%d %d %d %d %d\n", x[0], x[1], x[2], x[3], x[4]);
      }"""

    checkResults(code)
  }
  
//  "A array assignment with an init list of chars" should "print the correct results" in {
//    val code = """
//      void main() {
//        char x[5] = {'a', 'b', 'c', 'd', 'e'};
//        printf("%c %c %c %c %c\n", x[0], x[1], x[2], x[3], x[4]);
//      }"""
//
//    checkResults(code)
//  }
}

class ArrayTest extends StandardTest {
  "A trivial array assignment" should "print the correct results" in {
    val code = """
      void main() {
        int x[5];
        x[2] = 5;
        printf("%d\n", x[2]);
      }"""

    checkResults(code)
  }
  
  "A trivial array binary expression" should "print the correct results" in {
    val code = """
      void main() {
        int x[5];
        x[2] = 5;
        x[3] = 3;
        printf("%d\n", x[2] * x[3]);
      }"""

    checkResults(code)
  }
  
  "An array subscript with advanced binary expression" should "print the correct results" in {
    val code = """
      void main() {
        int x[5];
        int y = 2;
        x[1] = 3;
        x[3] = 12;
        printf("%d\n", x[y - 2 + x[1]]);
      }"""

    checkResults(code)
  }
  
  "An array prefixed subscript" should "print the correct results" in {
    val code = """
      void main() {
        int x[5] = {3, 68, 44, 29, 45};
        int y = 0;
        printf("%d %d\n", x[++y], y);
      }"""

    checkResults(code)
  }
  
  "An array postfixed subscript" should "print the correct results" in {
    val code = """
      void main() {
        int x[5] = {3, 68, 44, 29, 45};
        int y = 0;
        printf("%d %d\n", x[y++], y);
      }"""

    checkResults(code)
  }
}
