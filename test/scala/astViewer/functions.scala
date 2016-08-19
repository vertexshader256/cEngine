package scala.astViewer

class SizeOfTest extends StandardTest {

  "A sizeof call on an int var" should "print the correct results" in {
    val code = """
      void main() {
        int x;
        double y;
        printf("%d %d\n", sizeof(x), sizeof(y));
      }"""

    checkResults(code)
  }
  
  "A sizeof call on an int type" should "print the correct results" in {
    val code = """
      void main() {
        printf("%d %d\n", sizeof(int), sizeof(double));
      }"""

    checkResults(code)
  }
  
  "A sizeof call on an array type" should "print the correct results" in {
    val code = """
      void main() {
        int x[5];
        char y[5];
        printf("%d %d\n", sizeof(x), sizeof(y));
      }"""

    checkResults(code)
  }
}

class FunctionTest extends StandardTest {

  "A function prototype" should "print the correct results" in {
    val code = """
      int x = 5;
      void test();

      void main() {
        test();
        x = x + 1;
        printf("%d\n", x);
      }
      void test() {
         x = 10;
      }"""

    checkResults(code)
  }

  "A simple function call testing return point" should "print the correct results" in {
    val code = """
      int x = 5;
      void test() {
        x = 10;
      }
      void main() {
        test();
        x = x + 1;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }

  "chained function calls" should "print the correct results" in {
    val code = """
      int x = 5;
      void test2() {
        x = 10;
      }
      void test() {
        test2();
        x = x + 5;
      }
      void main() {
        test();
        printf("%d\n", x);
      }"""

    checkResults(code)
  }

  "a function with a return value" should "print the correct results" in {
    val code = """
      int test() {
        return 10;
      }
      void main() {
        printf("%d\n", test());
      }"""

    checkResults(code)
  }

  "chained return value" should "print the correct results" in {
    val code = """
      int test() {
        return 8;
      }
      int test2() {
        return test();
      }
      void main() {
        printf("%d\n", test2());
      }"""

    checkResults(code)
  }

  "a function with a complex return value" should "print the correct results" in {
    val code = """
      int test() {
        return (1 + 2) * (2 + 6);
      }
      void main() {
        printf("%d\n", test());
      }"""

    checkResults(code)
  }

  "a function with a argument" should "print the correct results" in {
    val code = """
      int square(int x) {
        return x * x;
      }
      void main() {
        printf("%d\n", square(5));
      }"""

    checkResults(code)
  }

  "a function with two arguments" should "print the correct results" in {
    val code = """
      int add(int x, int y) {
        return x + y;
      }
      void main() {
        printf("%d\n", add(13, 26));
      }"""

    checkResults(code)
  }

  "complex function calls" should "print the correct results" in {
    val code = """
      int add(int x, int y) {
        return x + y;
      }
      int square(int x) {
        return x * x;
      }
      void main() {
        printf("%d\n", square(5) + square(2));
        printf("%d\n", add(13, add(1, 5)));
        printf("%d\n", add(add(1, 5), 13));
        printf("%d\n", add(add(1, add(3, 2)), add(add(5, 5), 3)));
      }"""

    checkResults(code)
  }
}

class ComplexFunctionTest extends StandardTest {
  "a function with out-of-order references to arguments" should "print the correct results" in {
    val code = """
      int func(int x, int y) {
        int z = x + 3;
        int x2 = x - 2;
        int x3 = y - x;
        return x3 + x2 + z;
      }
      void main() {
        printf("%d\n", func(1, 2));
      }"""

    checkResults(code)
  }
}