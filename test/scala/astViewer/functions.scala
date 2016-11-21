package scala.astViewer

class FunctionStagingArea extends StandardTest {
  "An unsigned pointer argument" should "print the correct results" in {
    val code = """
      void test2(unsigned int *x) {
        printf("%d\n", x);
      }

      void main() {
        int a = 5;
        unsigned int *x = (unsigned int*) a;
        test2(x);
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
  
  "A early return" should "print the correct results" in {
    val code = """
      void test2() {
        printf("1\n");
        return;
        printf("2\n");
      }

      void main() {
        test2();
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
    
    val code2 = """
      double test() {
        return 10.5;
      }
      void main() {
        printf("%f\n", test());
      }"""

    checkResults(code2)
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
  
  "a function with a string as an argument" should "print the correct results" in {
    val code = """
      void thePrint(char *str) {
        printf(str);
      }
      void main() {
        thePrint("Test\n");
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
  
  "returning a variable value" should "print the correct results" in {
    val code = """

      double y = 4.56;
      double ret1(double x)
      {
        double rt = 1;
        return rt;
      }
      
      double ret2(double x)
      {
        return x;
      }
      
      double ret3()
      {
        return y;
      }
  
      void main()
      {
        printf("%f\n",ret1(9.0));
        printf("%f\n",ret2(3.0));
        printf("%f\n",ret3());
        return 0;
      }
      """

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
  
  "a function with double pointers as arguments" should "print the correct results" in {
    val code = """
      int func(int **x) {
        (**x)++;
      }
      void main() {
        int a = 0;
        int *a2 = &a;
        func(&a2);
        printf("%d\n", a);
      }"""

    checkResults(code)
  }
}