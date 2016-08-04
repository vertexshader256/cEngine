package scala.astViewer

import org.scalatest._

class FunctionTest extends FlatSpec with ShouldMatchers {

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

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("11"))
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

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("11"))
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

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("15"))
  }

  "a function with a return value" should "print the correct results" in {
    val code = """
      int test() {
        return 10;
      }
      void main() {
        printf("%d\n", test());
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("10"))
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

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("8"))
  }

  "a function with a complex return value" should "print the correct results" in {
    val code = """
      int test() {
        return (1 + 2) * (2 + 6);
      }
      void main() {
        printf("%d\n", test());
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("24"))
  }

  "a function with a argument" should "print the correct results" in {
    val code = """
      int square(int x) {
        return x * x;
      }
      void main() {
        printf("%d\n", square(5));
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("25"))
  }

  "a function with two arguments" should "print the correct results" in {
    val code = """
      int add(int x, int y) {
        return x + y;
      }
      void main() {
        printf("%d\n", add(13, 26));
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("39"))
  }

  "a binary expression with function calls" should "print the correct results" in {
    val code = """
      int square(int x) {
        return x * x;
      }
      void main() {
        printf("%d\n", square(5) + square(2));
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("29"))
  }
}