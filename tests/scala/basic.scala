package tests.scala

import tests.scala.TestClasses._

class AssignmentTest extends StandardTest {
  "A simple math expression with double assigned to integer" should "print the correct results" in {
    val code = """
      void main() {
        double x = 1;
        double y = 2;
        x = x + y;
        printf("%f\n", x);
      }"""

    checkResults(code)
  }
  
  "initialized by an assignment" should "print the correct results" in {
    val code = """
      void main() {
        double x = 1;
        double y = x;
        printf("%f\n", x);
      }"""

    checkResults(code)
  }
}

class PreprocessorTest extends StandardTest {
  "A simple math expression with double assigned to integer" should "print the correct results" in {
    val code = """
      
      #define PRINT(x) printf("%d\n", x)
      
      void main() {
        int x = 54332;
        PRINT(x);
      }"""

    checkResults(code)
  }
}

class BasicHelloWorld extends StandardTest {
  
  "Hello world" should "print the correct results" in {
    val code =
      """

      void main() {
        //init_printf(0,myputC);
        //tfp_printf("HELLO\n");
        printf("Hello World!\n");
      }"""

    checkResults(code)
  }
}

class SimpleInitTest extends StandardTest {

  "A simple integer initialized global reference" should "print the correct results" in {
    val code = """
      int x = 1;
      void main() {
        printf("%d\n", x);
      }"""
    
    checkResults(code)
  }
}

class ComplexInitTest extends StandardTest {
  "A simple function-scoped multi-var init" should "print the correct results" in {
    val code = """
      void main() {
        int x = 454, y = 65, z = 23;
        printf("%d %d %d\n", x, y, z);
      }"""

    checkResults(code)
  }
  
  "A cascaded multi-var init" should "print the correct results" in {
    val code = """
      void main() {
        int x, y, z;
        x = y = z = 30;
        printf("%d %d %d\n", x, y, z);
      }"""

    checkResults(code)
  }
}

class BasicTest extends StandardTest {

  "main with arguments" should "print the correct results" in {
    val code = """
      int main(int argc, char *argv[])
      {
      	printf("Hello world!\n");
      	return 0;
      }"""

    checkResults(code)
  }
  
  "A simple integer uninitialized global reference" should "print the correct results" in {
    val code = """
      int x;

      void main() {
        x = 2;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }

  "A simple integer uninitialized local reference" should "print the correct results" in {
    val code = """
      void main() {
        int x;
        x = 2;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }

  "A simple function-scoped integer reference" should "print the correct results" in {
    val code = """
      void main() {
        int x = 1;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }

  "A simple math expression with addition and one inner var" should "print the correct results" in {
    val code = """
      void main() {
        int x = 1 + 2;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "A simple math expression with double reassignment" should "print the correct results" in {
    val code = """
      void main() {
        double x = 1.4;
        double y = 2.4;
        x = x + y;
        printf("%f\n", x);
      }"""

    checkResults(code)
  }
  
  "A simple math expression with addition and one global var" should "print the correct results" in {
    val code = """
      int x = 1 + 2;

      void main() {
        printf("%d\n", x);
      }"""

    checkResults(code)
  }

  "A simple math expression with addition and two global vars" should "print the correct results" in {
    val code = """
      int x = 1 + 2;
      int y = 5 - 3;

      void main() {
        printf("%d\n", x * y);
      }"""

    checkResults(code)
  }

  "A simple inlined math expression with addition" should "print the correct results" in {
    val code = """
      void main() {
        printf("%d\n", 1 + 2);
      }"""

    checkResults(code)
  }

  "A simple math expression with addition and two variables" should "print the correct results" in {
    val code = """
      void main() {
        int x = 4;
        int y = 3;
        printf("%d\n", x + y);
      }"""

    checkResults(code)
  }

  "A simple math expression with addition, a variable, and a literal" should "print the correct results" in {
    val code = """
      void main() {
        int x = 4;
        printf("%d\n", x + 4);
      }"""

    checkResults(code)
  }

  "A simple 3-literal math expression" should "print the correct results" in {
    val code = """
      void main() {
        int x = 1 + 2 + 3;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }

  "A simple math expression with substraction" should "print the correct results" in {
    val code = """
      void main() {
        int x = 10 - 7;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }

  "A simple math expression with multiplication" should "print the correct results" in {
    val code = """
      void main() {
        int x = 10 * 7;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }

  "A simple math expression with division" should "print the correct results" in {
    val code = """
      void main() {
        int x = 27 / 3;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }

  "Order of operations test 1" should "print the correct results" in {
    val code = """
      void main() {
        int x = 1 * 2 + 3;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }

  "Order of operations test 2" should "print the correct results" in {
    val code = """
      void main() {
        int x = 1 + 2 * 3;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }

  "Order of operations test 3" should "print the correct results" in {
    val code = """
      void main() {
        int x = (1 + 2) * 3;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }

  "A simple local variable reassignment" should "print the correct results" in {
    val code = """
      void main() {
        int x = 10;
        x = 5;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  "A simple local variable reassignment after a switch statement" should "print the correct results" in {
    val code = """
      void main() {
        int x = 10;
        switch(x) {
          default :
            printf("2\n");
            break;
        }
        x = 5;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }

  "A more complex local variable reassignment" should "print the correct results" in {
    val code = """
      void main() {
        int x = 10;
        int y = 3;
        x = 5 * y;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
}