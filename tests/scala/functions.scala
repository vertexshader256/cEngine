package tests.scala

import tests.scala.TestClasses._

import scala.c_engine.State

class FunctionStackTest extends StandardTest {
  "ensure the stack gets popped after functions" should "print the correct results" in {
    import scala.c_engine._
    import scala.c_engine.cEngine._
    implicit val state = new State

    val start = state.stackInsertIndex

    c"""isalpha('5');"""

    assert(state.stackInsertIndex == start)
  }

  "ensure the stack gets popped after functions with string args" should "print the correct results" in {
    import scala.c_engine._
    import scala.c_engine.cEngine._
    implicit val state = new State

    val start = state.stackInsertIndex

    c"""printf("%s\n", "whoaaaa");"""

    assert(state.stackInsertIndex == start)
  }
}

class SpecialFunctions extends StandardTest {

  "offsetof test" should "print the correct results" in {
    val code = """
      #include <stdio.h>
      #include "stddef.h"
      #include "stdint.h"

      struct test {
        int x;
        int y;
        int z;
      };

      struct test2 {
         int x;
         int* y;
         int z;
       };

      int main()
      {
          printf("%d\n", offsetof(struct test, y));
          printf("%d\n", offsetof(struct test2, z));
      }"""

    checkResults(code)
  }
}

class FunctionPointerTest extends StandardTest {
  "a function pointer test" should "print the correct results" in {
    val code = """
      #include <stdio.h>
      void my_int_func(int x)
      {
          printf( "%d\n", x );
      }
      
      
      int main()
      {
          void (*foo)(int);
          foo = &my_int_func;
      
          /* call my_int_func (note that you do not need to write (*foo)(2) ) */
          foo( 2 );
          /* but if you want to, you may */
          (*foo)( 2 );
      
          return 0;
      }
      
    """
    
    checkResults(code)
  }
  
  "an inner-struct function pointer to scala function" should "print the correct results" in {
    val code = """
      #include <stdio.h>
      #include <string.h>
      
      typedef struct blah {
        int (*foo)(int);
      } blah;
      
      int main()
      {
          blah x;
          x.foo = &strlen;

          printf("%d\n", x.foo("this is just a test"));
      
          return 0;
      }
      
    """
    
    checkResults(code)
  }
  
  "array of function pointers" should "print the correct results" in {
    val code = """
      typedef void (*fp)(int); //Declares a type of a void function that accepts an int

      void test(int i)
      {
          printf("%d\n", i);
      }
      
      void test2(int i)
      {
          printf("%d\n", i*i);
      }
      
      fp initArray2[] = {test, test2};
      
      int main(int argc, void* argv[])
      {
          fp function_array[2];
          fp initArray[2] = {test, test2};
      
          function_array[0] = test;
          function_array[1] = test2;
      
          function_array[0](10);
          function_array[1](7);
          
          function_array[0] = test2;
          
          function_array[0](10);
          
          initArray[0](5);
          initArray[1](11);
          
          initArray2[0](15);
          initArray2[1](13);
      }
      
    """
    
    checkResults(code)
  }
  
  
}

class VarArgFunction extends StandardTest {
  "A function with variable arguments" should "print the correct results" in {
    val code = """
      #include <stdio.h>
      #include <stdarg.h>

      double average(int num,...) {
         double sum = 0.0;
         va_list valist;
         int i;
      
         /* initialize valist for num number of arguments */
         va_start(valist, num);
         
         printf("%d\n", num);
      
         /* access all the arguments assigned to valist */
         for (i = 0; i < num; i++) {
            int j = va_arg(valist, int);
            printf("%d\n", j);
            sum += j;
         }
      	
         /* clean memory reserved for valist */
         va_end(valist);
      
         return sum/num;
      }
      
      int main() {
         int x = 10;
         int y = 101;
         printf("%f\n", average(4, 2,3,4,5));
         printf("%f\n", average(3, 5,10,15));
         printf("%f\n", average(2, x, y));
         printf("%f\n", average(2, x + y, y - x));
      }"""

    checkResults(code)
  }
}

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