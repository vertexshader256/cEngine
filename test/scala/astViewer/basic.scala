package scala.astViewer

import org.scalatest._

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._

class HelloWorld extends FlatSpec with ShouldMatchers {

  "Hello world" should "print the correct results" in {
    val code =
      """
      void main() {
        printf("%s\n", "Hello world!");
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal(Some("Hello world!"))
  }
}

class BasicTest extends FlatSpec with ShouldMatchers {

  "A simple integer initialized global reference" should "print the correct results" in {
    val code = """
      int x = 1;
      void main() {
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("1"))
  }

  "A simple integer uninitialized global reference" should "print the correct results" in {
    val code = """
      int x;

      void main() {
        x = 2;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("2"))
  }

  "A simple integer uninitialized local reference" should "print the correct results" in {
    val code = """
      void main() {
        int x;
        x = 2;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("2"))
  }

  "A simple function-scoped integer reference" should "print the correct results" in {
    val code = """
      void main() {
        int x = 1;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("1"))
  }

  "A simple math expression with addition and one inner var" should "print the correct results" in {
    val code = """
      void main() {
        int x = 1 + 2;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("3"))
  }

  "A simple math expression with addition and one global var" should "print the correct results" in {
    val code = """
      int x = 1 + 2;

      void main() {
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("3"))
  }

  "A simple math expression with addition and two global vars" should "print the correct results" in {
    val code = """
      int x = 1 + 2;
      int y = 5 - 3;

      void main() {
        printf("%d\n", x * y);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("6"))
  }

  "A simple inlined math expression with addition" should "print the correct results" in {
    val code = """
      void main() {
        printf("%d\n", 1 + 2);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("3"))
  }

  "A simple math expression with addition and two variables" should "print the correct results" in {
    val code = """
      void main() {
        int x = 4;
        int y = 3;
        printf("%d\n", x + y);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("7"))
  }

  "A simple math expression with addition, a variable, and a literal" should "print the correct results" in {
    val code = """
      void main() {
        int x = 4;
        printf("%d\n", x + 4);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("8"))
  }

  "A simple 3-literal math expression" should "print the correct results" in {
    val code = """
      void main() {
        int x = 1 + 2 + 3;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("6"))
  }

  "A simple math expression with substraction" should "print the correct results" in {
    val code = """
      void main() {
        int x = 10 - 7;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("3"))
  }

  "A simple math expression with multiplication" should "print the correct results" in {
    val code = """
      void main() {
        int x = 10 * 7;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("70"))
  }

  "A simple math expression with division" should "print the correct results" in {
    val code = """
      void main() {
        int x = 27 / 3;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("9"))
  }

  "Order of operations test 1" should "print the correct results" in {
    val code = """
      void main() {
        int x = 1 * 2 + 3;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("5"))
  }

  "Order of operations test 2" should "print the correct results" in {
    val code = """
      void main() {
        int x = 1 + 2 * 3;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("7"))
  }

  "Order of operations test 3" should "print the correct results" in {
    val code = """
      void main() {
        int x = (1 + 2) * 3;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("9"))
  }

  "A simple local variable reassignment" should "print the correct results" in {
    val code = """
      void main() {
        int x = 10;
        x = 5;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("5"))
  }

  "A more complex local variable reassignment" should "print the correct results" in {
    val code = """
      void main() {
        int x = 10;
        int y = 3;
        x = 5 * y;
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("15"))
  }
}