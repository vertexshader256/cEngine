package scala.astViewer

import org.scalatest._

class FunctionTest extends FlatSpec with ShouldMatchers {

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

//  "a function with a return value" should "print the correct results" in {
//    val code = """
//      int test() {
//        return 10;
//      }
//      void main() {
//        printf("%d\n", test());
//      }"""
//
//    val executor = new Executor(code)
//    executor.execute
//    executor.stdout.headOption should equal (Some("10"))
//  }
}