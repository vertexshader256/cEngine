package scala.astViewer

import org.scalatest._
import scala.reflect.runtime.universe._
import org.eclipse.cdt.internal.core.dom.parser.c._
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.parser._
import scala.collection.mutable.ListBuffer
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._

trait PrimitiveType
case class IntPrimitive(name: String, value: Int) extends PrimitiveType

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

class Executor {
  
  val stdout = new ListBuffer[String]();
  
  def parseIntExpr(decl: IASTExpression, scope: Scope): Int = {
    decl match {
      case bin: IASTBinaryExpression =>
        val (op1, op2) = (bin.getOperand1, bin.getOperand2)
        bin.getOperator match {
          case `op_plus` => parseIntExpr(op1, scope) + parseIntExpr(op2, scope)
          case `op_minus` => parseIntExpr(op1, scope) - parseIntExpr(op2, scope)
          case `op_multiply` => parseIntExpr(op1, scope) * parseIntExpr(op2, scope)
          case `op_divide` => parseIntExpr(op1, scope) / parseIntExpr(op2, scope)
        }
      case unary: IASTUnaryExpression =>
        parseIntExpr(unary.getOperand, scope)
      case lit: IASTLiteralExpression =>
        lit.getRawSignature.toInt
      case variable: CASTIdExpression =>
        scope.getVariableValue(variable.getName.getRawSignature).toInt    
    }
  }
  
  def parseDeclaration(decl: IASTDeclaration, scope: Scope) = {
    decl match {
      case simple: IASTSimpleDeclaration =>
        val declarator = simple.getDeclarators.head // fix: assuming only 1
        var declaratorName = declarator.getName.getRawSignature
        
        declarator.getInitializer match {
          case equals: CASTEqualsInitializer => // being assigned
            
            simple.getDeclSpecifier.getRawSignature match {
              case "int" => scope.integers += IntPrimitive(declaratorName, parseIntExpr(equals.getExpression, scope))
            }
        }
    }
  }
  
  def executeStatement(statement: IASTStatement, outerScope: Scope) = {
    val scope = new Scope(outerScope)
    
    statement match {
      case compound: CASTCompoundStatement => {
        compound.getStatements.foreach { statement =>
          statement match {
            case decl: CASTDeclarationStatement =>
              parseDeclaration(decl.getDeclaration, scope)
            case expr: CASTExpressionStatement => {
              expr.getExpression match {
                case call: CASTFunctionCallExpression => {
                  if (call.getFunctionNameExpression.getRawSignature == "printf") {
                    val formatArg = call.getArguments.head.getRawSignature.tail.reverse.tail.reverse // first arg is the format

                    call.getArguments.tail.foreach{arg =>
                      arg match {
                        case x: CASTIdExpression => stdout += scope.getVariableValue(x.getName.getRawSignature)
                        case expr: IASTBinaryExpression => stdout += parseIntExpr(expr, scope).toString
                        case lit: IASTLiteralExpression => {
                          val arg = lit.getRawSignature

                          if (formatArg.contains("%s") && arg.head == '\"' && arg.last == '\"') {
                            val noQuotes = arg.tail.reverse.tail.reverse
                            stdout += formatArg.replace("%s", noQuotes).replace("\\n", "")
                          } else {
                            Unit
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          } 
        }
      }
    }
  }
  
  def executeFunction(fcnDef: IASTFunctionDefinition, outerScope: Scope) = {
    executeStatement(fcnDef.getBody, outerScope)
  }
  
  def execute(tUnit: IASTTranslationUnit) = {

    var main: CASTFunctionDefinition = null
    val globalScope = new Scope(null)

    tUnit.getDeclarations.foreach{ decl =>
      decl match {
        case x: CASTFunctionDefinition => {
          if (x.getDeclarator.getName.getRawSignature == "main") {
            main = x
          }
        }
        case x: CASTSimpleDeclaration => parseDeclaration(x, globalScope)
      }
    }

    executeFunction(main, globalScope)
  }
  
}

class BasicTest extends FlatSpec with ShouldMatchers {

  def resolveDeclStatement(statement: CASTDeclarationStatement) = {
    statement.getDeclaration match {
      case simple: CASTSimpleDeclaration =>
        
    }
  }

  "Hello world" should "print the correct results" in {
    val tUnit = AstUtils.getTranslationUnit("""
      void main() {
        printf("%s\n", "Hello world!");
      }""")
    val executor = new Executor
    executor.execute(tUnit)
    executor.stdout.head should equal ("Hello world!")
  }

  "A simple math expression with addition and one inner var" should "print the correct results" in {
    val tUnit = AstUtils.getTranslationUnit("""
      void main() {
        int x = 1 + 2;
        printf("%d\n", x);
      }""")

    val executor = new Executor
    executor.execute(tUnit)
    executor.stdout.head should equal ("3")
  }

  "A simple math expression with addition and one global var" should "print the correct results" in {
    val tUnit = AstUtils.getTranslationUnit("""
      int x = 1 + 2;

      void main() {
        printf("%d\n", x);
      }""")

    val executor = new Executor
    executor.execute(tUnit)
    executor.stdout.head should equal ("3")
  }

  "A simple math expression with addition and two global vars" should "print the correct results" in {
    val tUnit = AstUtils.getTranslationUnit("""
      int x = 1 + 2;
      int y = 5 - 3;

      void main() {
        printf("%d\n", x * y);
      }""")

    val executor = new Executor
    executor.execute(tUnit)
    executor.stdout.head should equal ("6")
  }
  
  "A simple inlined math expression with addition" should "print the correct results" in {
    val tUnit = AstUtils.getTranslationUnit("""
      void main() {
        printf("%d\n", 1 + 2);
      }""")
      
      val executor = new Executor
      executor.execute(tUnit)     
      executor.stdout.head should equal ("3")              
  }
  
  "A simple math expression with addition and two variables" should "print the correct results" in {
    val tUnit = AstUtils.getTranslationUnit("""
      void main() {
        int x = 4;
        int y = 3;
        printf("%d\n", x + y);
      }""")
      
      val executor = new Executor
      executor.execute(tUnit)     
      executor.stdout.head should equal ("7")              
  }

  "A simple math expression with addition, a variable, and a literal" should "print the correct results" in {
    val tUnit = AstUtils.getTranslationUnit("""
      void main() {
        int x = 4;
        printf("%d\n", x + 4);
      }""")

    val executor = new Executor
    executor.execute(tUnit)
    executor.stdout.head should equal ("8")
  }
  
  "A simple 3-literal math expression" should "print the correct results" in {
    val tUnit = AstUtils.getTranslationUnit("""
      void main() {
        int x = 1 + 2 + 3;
        printf("%d\n", x);
      }""")
      
      val executor = new Executor
      executor.execute(tUnit)     
      executor.stdout.head should equal ("6")              
  }
  
  "A simple math expression with substraction" should "print the correct results" in {
    val tUnit = AstUtils.getTranslationUnit("""
      void main() {
        int x = 10 - 7;
        printf("%d\n", x);
      }""")
      
      val executor = new Executor
      executor.execute(tUnit)     
      executor.stdout.head should equal ("3")              
  }
  
  "A simple math expression with multiplication" should "print the correct results" in {
    val tUnit = AstUtils.getTranslationUnit("""
      void main() {
        int x = 10 * 7;
        printf("%d\n", x);
      }""")
      
      val executor = new Executor
      executor.execute(tUnit)     
      executor.stdout.head should equal ("70")              
  }
  
  "A simple math expression with division" should "print the correct results" in {
    val tUnit = AstUtils.getTranslationUnit("""
      void main() {
        int x = 27 / 3;
        printf("%d\n", x);
      }""")
      
      val executor = new Executor
      executor.execute(tUnit)     
      executor.stdout.head should equal ("9")              
  }
  
  "Order of operations test 1" should "print the correct results" in {
    val tUnit = AstUtils.getTranslationUnit("""
      void main() {
        int x = 1 * 2 + 3;
        printf("%d\n", x);
      }""")
      
      val executor = new Executor
      executor.execute(tUnit)     
      executor.stdout.head should equal ("5")              
  }
  
  "Order of operations test 2" should "print the correct results" in {
    val tUnit = AstUtils.getTranslationUnit("""
      void main() {
        int x = 1 + 2 * 3;
        printf("%d\n", x);
      }""")
      
      val executor = new Executor
      executor.execute(tUnit)     
      executor.stdout.head should equal ("7")              
  }
  
  "Order of operations test 3" should "print the correct results" in {
    val tUnit = AstUtils.getTranslationUnit("""
      void main() {
        int x = (1 + 2) * 3;
        printf("%d\n", x);
      }""")
      
      val executor = new Executor
      executor.execute(tUnit)     
      executor.stdout.head should equal ("9")              
  }
}