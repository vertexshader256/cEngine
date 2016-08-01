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
  val functions = Utils.findFunctions(tUnit)
  val main = functions.filter{fcn => fcn.getDeclarator.getName.getRawSignature == "main"}.head

  val stdout = new ListBuffer[String]()
  
  var nestingLevel = 0
  var inFunction = false
  var inFunctionCall = false
  var functionBeingCalled = ""

  var currentScope: IScope = tUnit.getScope
  var isLeavingNode = false

 // var main: IASTFunctionDefinition = null
  val globalScope = new Scope(null)

  val path = Utils.getPath(tUnit)

  var isInDeclaration = false
  var isInFunctionCallExpr = false
  var isInDeclarator = false

  val integerStack = new Stack[Int]()
  val variableMap = scala.collection.mutable.Map[String, IntPrimitive]()

  def step(node: IASTNode) = {
    node match {
      case tUnit: IASTTranslationUnit =>
      case simple: IASTSimpleDeclaration =>
        isInDeclaration = !isInDeclaration
      case fcnDec: IASTFunctionDeclarator =>
      case decl: IASTDeclarator =>
        isInDeclarator = !isInDeclarator
        if (!isInDeclarator) {
          val value = integerStack.pop
          println("ADDING VAR: " + decl.getName.getRawSignature + ", " + value)
          variableMap += (decl.getName.getRawSignature -> IntPrimitive(decl.getName.getRawSignature, value))
        }
      case fcnDef: IASTFunctionDefinition =>
        if (!isLeavingNode) {
          currentScope = fcnDef.getScope
        }
      case decl: IASTSimpleDeclaration =>
      case call: IASTFunctionCallExpression =>

        isInFunctionCallExpr = !isInFunctionCallExpr

        // only evaluate after leaving
        if (!isInFunctionCallExpr || call.getArguments()(1).isInstanceOf[IASTLiteralExpression] || call.getArguments()(1).isInstanceOf[IASTIdExpression]) {
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
          }
        }


      case lit: IASTLiteralExpression =>
      case decl: IASTDeclarationStatement =>
      case compound: IASTCompoundStatement =>
        if (!isLeavingNode) {
          currentScope = compound.getScope
        }
      case exprStatement: IASTExpressionStatement =>
      case equalsInit: IASTEqualsInitializer =>
        equalsInit.getInitializerClause match {
          case lit: IASTLiteralExpression =>
            integerStack.push(lit.getRawSignature.toInt)
          case _ =>
        }
      case binaryExpr: IASTBinaryExpression =>

        val op1 = (binaryExpr.getOperand1 match {
          case lit: IASTLiteralExpression => lit.getRawSignature.toInt
          case id: IASTIdExpression => variableMap(id.getRawSignature).value.toInt
        })

        val op2 = binaryExpr.getOperand2 match {
          case lit: IASTLiteralExpression => lit.getRawSignature.toInt
          case id: IASTIdExpression => variableMap(id.getRawSignature).value.toInt
        }

        val result = binaryExpr.getOperator match {
          case `op_multiply` =>
            op1 * op2
          case `op_plus` =>
            op1 + op2
          case `op_minus` =>
            op1 - op2
        }

        integerStack.push(result)
    }
  }

  def execute = {
    //path.foreach{ node => println(node.getClass.getSimpleName)}
    path.foreach{ node => step(node)}
  }
}

class BasicTest extends FlatSpec with ShouldMatchers {

  def resolveDeclStatement(statement: CASTDeclarationStatement) = {
    statement.getDeclaration match {
      case simple: CASTSimpleDeclaration =>
        
    }
  }

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

//  "A simple inlined math expression with addition" should "print the correct results" in {
//    val code = """
//      void main() {
//        printf("%d\n", 1 + 2);
//      }"""
//
//    val executor = new Executor(code)
//    executor.execute
//    executor.stdout.headOption should equal (Some("3"))
//  }
//
//  "A simple math expression with addition and two variables" should "print the correct results" in {
//    val code = """
//      void main() {
//        int x = 4;
//        int y = 3;
//        printf("%d\n", x + y);
//      }"""
//
//    val executor = new Executor(code)
//    executor.execute
//    executor.stdout.headOption should equal (Some("7"))
//  }
//
//  "A simple math expression with addition, a variable, and a literal" should "print the correct results" in {
//    val code = """
//      void main() {
//        int x = 4;
//        printf("%d\n", x + 4);
//      }"""
//
//    val executor = new Executor(code)
//    executor.execute
//    executor.stdout.headOption should equal (Some("8"))
//  }

//  "A simple 3-literal math expression" should "print the correct results" in {
//    val tUnit = AstUtils.getTranslationUnit("""
//      void main() {
//        int x = 1 + 2 + 3;
//        printf("%d\n", x);
//      }""")
//
//      val executor = new Executor
//      executor.execute(tUnit)
//      executor.stdout.head should equal ("6")
//  }
//  
//  "A simple math expression with substraction" should "print the correct results" in {
//    val tUnit = AstUtils.getTranslationUnit("""
//      void main() {
//        int x = 10 - 7;
//        printf("%d\n", x);
//      }""")
//      
//      val executor = new Executor
//      executor.execute(tUnit)     
//      executor.stdout.head should equal ("3")              
//  }
//  
//  "A simple math expression with multiplication" should "print the correct results" in {
//    val tUnit = AstUtils.getTranslationUnit("""
//      void main() {
//        int x = 10 * 7;
//        printf("%d\n", x);
//      }""")
//      
//      val executor = new Executor
//      executor.execute(tUnit)     
//      executor.stdout.head should equal ("70")              
//  }
//  
//  "A simple math expression with division" should "print the correct results" in {
//    val tUnit = AstUtils.getTranslationUnit("""
//      void main() {
//        int x = 27 / 3;
//        printf("%d\n", x);
//      }""")
//      
//      val executor = new Executor
//      executor.execute(tUnit)     
//      executor.stdout.head should equal ("9")              
//  }
//  
//  "Order of operations test 1" should "print the correct results" in {
//    val tUnit = AstUtils.getTranslationUnit("""
//      void main() {
//        int x = 1 * 2 + 3;
//        printf("%d\n", x);
//      }""")
//      
//      val executor = new Executor
//      executor.execute(tUnit)     
//      executor.stdout.head should equal ("5")              
//  }
//  
//  "Order of operations test 2" should "print the correct results" in {
//    val tUnit = AstUtils.getTranslationUnit("""
//      void main() {
//        int x = 1 + 2 * 3;
//        printf("%d\n", x);
//      }""")
//      
//      val executor = new Executor
//      executor.execute(tUnit)     
//      executor.stdout.head should equal ("7")              
//  }
//  
//  "Order of operations test 3" should "print the correct results" in {
//    val tUnit = AstUtils.getTranslationUnit("""
//      void main() {
//        int x = (1 + 2) * 3;
//        printf("%d\n", x);
//      }""")
//      
//      val executor = new Executor
//      executor.execute(tUnit)     
//      executor.stdout.head should equal ("9")              
//  }
}