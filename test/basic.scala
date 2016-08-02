package scala.astViewer

import java.io.File
import java.util.HashMap

import org.scalatest._

import scala.reflect.runtime.universe._
import org.eclipse.cdt.internal.core.dom.parser.c._
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.parser._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._

trait PrimitiveType
case class IntPrimitive(name: String, value: Long) extends PrimitiveType

class Scope(outerScope: Scope) {
  val integers = new ListBuffer[IntPrimitive]() 
  
  def getVariableValue(name: String): String = {
    if (outerScope != null) {
      (outerScope.integers ++ integers).filter(_.name == name).head.value.toString
    } else {
      integers.filter(_.name == name).head.value.toString
    }
  }
}

class Executor(code: String) {

  val tUnit = Utils.getTranslationUnit(code)

  val stdout = new ListBuffer[String]()
  var currentScope: IScope = tUnit.getScope
  var currentNode: IASTNode = null
  var currentIndex = 0

  val globalScope = new Scope(null)
  val path = Utils.getPath(tUnit)

  val integerStack = new Stack[Int]()
  val variableMap = scala.collection.mutable.Map[String, IntPrimitive]()
  val functionMap = scala.collection.mutable.Map[String, Int]()

  var functionReturnStack = new Stack[Int]()

  def step(current: Path, next: Path, wholePath: Seq[Path]) = {

    val direction = current.direction

    current.node match {
      case unary: IASTUnaryExpression =>
      case id: IASTIdExpression =>
      case tUnit: IASTTranslationUnit =>
      case simple: IASTSimpleDeclaration =>
      case fcnDec: IASTFunctionDeclarator =>
      case decl: IASTDeclarator =>
        if (direction == Exiting) {
          val value = integerStack.pop
          //println("ADDING VAR: " + decl.getName.getRawSignature + ", " + value)
          variableMap += (decl.getName.getRawSignature -> IntPrimitive(decl.getName.getRawSignature, value))
        }
      case fcnDef: IASTFunctionDefinition =>
        if (direction == Exiting) {
          if (!functionReturnStack.isEmpty) {
            // We are exiting a function we're currently executing
            currentIndex = functionReturnStack.pop
          }
        } else if (direction == Entering) {
          functionMap += (fcnDef.getDeclarator.getName.getRawSignature -> currentIndex)
        }
      case decl: IASTSimpleDeclaration =>
      case call: IASTFunctionCallExpression =>

        // only evaluate after leaving
        if (direction == Exiting) {
          val name = call.getFunctionNameExpression.getRawSignature
          val args = call.getArguments

          if (name == "printf") {
            val secondArg = args(1).getRawSignature
            if (secondArg.head == '\"' || secondArg.last == '\"') {
              println(args(1).getRawSignature)
              stdout += args(1).getRawSignature.tail.reverse.tail.reverse
            } else if (args(1).isInstanceOf[IASTBinaryExpression]) {
              // the argument is an expression
              stdout += integerStack.pop.toString
            } else {
              // the argument is just a variable reference
              stdout += variableMap(args(1).getRawSignature).value.toString
            }
          } else {
            functionReturnStack.push(currentIndex + 1)
            currentIndex = functionMap(name)
            //println("JUMPING TO: " + currentIndex)
          }
        }
      case lit: IASTLiteralExpression =>
      case decl: IASTDeclarationStatement =>
      case compound: IASTCompoundStatement =>
      case exprStatement: IASTExpressionStatement =>
      case equalsInit: IASTEqualsInitializer =>
        equalsInit.getInitializerClause match {
          case lit: IASTLiteralExpression =>
            integerStack.push(lit.getRawSignature.toInt)
          case _ => // dont do anything
        }
      case binaryExpr: IASTBinaryExpression =>

        if (direction == Exiting || direction == Visiting) {

          val op1 = (binaryExpr.getOperand1 match {
            case lit: IASTLiteralExpression => lit.getRawSignature.toInt
            case id: IASTIdExpression => variableMap(id.getRawSignature).value.toInt
            case bin: IASTBinaryExpression => integerStack.pop
            case bin: IASTUnaryExpression => integerStack.pop
          })

          val op2 = binaryExpr.getOperand2 match {
            case lit: IASTLiteralExpression => lit.getRawSignature.toInt
            case id: IASTIdExpression => variableMap(id.getRawSignature).value.toInt
            case bin: IASTBinaryExpression => integerStack.pop
            case bin: IASTUnaryExpression => integerStack.pop
          }

          binaryExpr.getOperator match {
            case `op_multiply` =>
              integerStack.push(op1 * op2)
            case `op_plus` =>
              integerStack.push(op1 + op2)
            case `op_minus` =>
              integerStack.push(op1 - op2)
            case `op_divide` =>
              integerStack.push(op1 / op2)
            case `op_assign` =>
              variableMap += (binaryExpr.getOperand1.getRawSignature -> IntPrimitive(binaryExpr.getOperand1.getRawSignature, op2))
          }
        }
    }
  }

  def execute = {
    //path.foreach{ node => println(node.getClass.getSimpleName)}



    var isDone = false
    var currentNode = path.head.node

    while (!isDone) {
      if (currentIndex == path.size - 1) {
        step(path(currentIndex), null, path)
        currentIndex += 1
      } else if (currentIndex == path.size) {
        isDone = true
      } else {
        step(path(currentIndex), path(currentIndex + 1), path)
        currentIndex += 1
      }
    }
  }
}

class BasicTest extends FlatSpec with ShouldMatchers {

  "Hello world" should "print the correct results" in {
    val code = """
      void main() {
        printf("%s\n", "Hello world!");
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("Hello world!"))
  }

  "A simple integer global reference" should "print the correct results" in {
    val code = """
      int x = 1;
      void main() {
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("1"))
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

  "A simple function call" should "print the correct results" in {
    val code = """
      int x = 5;
      void test() {
        x = 10;
      }
      void main() {
        test();
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("10"))
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
}