package scala.astViewer

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._
import org.eclipse.cdt.core.dom.ast.{IASTEqualsInitializer, _}

import scala.astViewer.{IntPrimitive, Path, Utils}
import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch



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

case class IASTContext(startNode: IASTNode) {
  var currentPath: Path = null
  val stack = new Stack[Any]()
  val variableMap = scala.collection.mutable.Map[String, Any]()
  val path = Utils.getPath(startNode)
}

class Executor(code: String) {

  var isPreprocessing = true
  val tUnit = Utils.getTranslationUnit(code)

  val stdout = new ListBuffer[String]()

  val mainContext = new IASTContext(tUnit)

  val functionMap = scala.collection.mutable.Map[String, IASTNode]()

  var functionReturnStack = new Stack[IASTFunctionCallExpression]()
  val functionArgumentMap = scala.collection.mutable.Map[String, Any]()

  var isVarInitialized = false
  var arraySize = 0

  def isLongNumber(s: String): Boolean = (allCatch opt s.toLong).isDefined
  def isDoubleNumber(s: String): Boolean = (allCatch opt s.toDouble).isDefined

  def printf(context: IASTContext) = {
    var current = context.stack.pop
    val formatString = current.asInstanceOf[String].replaceAll("^\"|\"$", "")


    def getNumericArg() = {
      context.stack.pop
    }

    def getStringArg() = {
      current = context.stack.pop
      val arg = current.asInstanceOf[String].replaceAll("^\"|\"$", "")
      arg
    }

    val result = formatString.split("""%d""").reduce{_ + getNumericArg + _}
      .split("""%s""").reduce{_ + getStringArg + _}
      .split("""%f""").reduce{_ + getNumericArg + _}

    result.split("""\\n""").foreach(line => stdout += line)
  }

  def parseStatement(statement: IASTStatement, context: IASTContext, direction: Direction): Seq[IASTNode] = statement match {
    case ifStatement: IASTIfStatement =>
      if (direction == Entering) {
        Seq(ifStatement.getConditionExpression)
      } else {
        val conditionResult = context.stack.pop match {
          case x: Int => x == 1
          case x: Boolean => x
        }
        if (conditionResult) {
          Seq(ifStatement.getThenClause)
        } else {
          Seq(ifStatement.getElseClause)
        }
      }
    case ret: IASTReturnStatement =>
      if (direction == Entering) {
        Seq(ret.getReturnValue)
      } else {
        Seq()
      }
    case decl: IASTDeclarationStatement =>
      if (direction == Entering) {
        Seq(decl.getDeclaration)
      } else {
        Seq()
      }
    case compound: IASTCompoundStatement =>
      if (direction == Entering) {
        compound.getStatements
      } else {
        Seq()
      }
    case exprStatement: IASTExpressionStatement =>
      if (direction == Entering) {
        Seq(exprStatement.getExpression)
      } else {
        Seq()
      }
  }

  def parseExpression(expr: IASTExpression, direction: Direction, context: IASTContext): Seq[IASTNode] = expr match {
    case subscript: IASTArraySubscriptExpression =>
      Seq()
    case unary: IASTUnaryExpression =>
      if (direction == Entering) {
        Seq(unary.getOperand)
      } else {
        Seq()
      }
    case lit: IASTLiteralExpression =>
      //   HACK ALERT
      //   FIX THIS (dont have it specific to IF statements)
      //if (lit.getParent.isInstanceOf[IASTIfStatement]) {
      if (direction == Exiting) {
        println("PUSHING LIT: " + castLiteral(lit))
        context.stack.push(castLiteral(lit))
      }
      Seq()
    case id: IASTIdExpression =>
      if (direction == Exiting) {
        context.stack.push(if (context.variableMap.contains(id.getRawSignature)) {
          context.variableMap(id.getRawSignature)
        } else {
          functionArgumentMap(id.getRawSignature)
        })
      }
      Seq()
    case call: IASTFunctionCallExpression =>
      // only evaluate after leaving
      if (direction == Exiting) {
        val name = call.getFunctionNameExpression match {
          case x: IASTIdExpression => x.getName.getRawSignature
          case _ => "Error"
        }

        if (name == "printf") {
          printf(context)
          Seq()
        } else {
          functionReturnStack.push(call)
          Seq(functionMap(name))
        }

      } else {
        call.getArguments.reverse
      }

    case bin: IASTBinaryExpression =>
      if (direction == Exiting) {
        val result = parseBinaryExpr(bin, direction, context)
        if (result != null) {
          context.stack.push(result)
        }
        Seq()
      } else {
        Seq(bin.getOperand1, bin.getOperand2)
      }
  }

  def step(current: IASTNode, context: IASTContext, direction: Direction): Seq[IASTNode] = {

    current match {
      case statement: IASTStatement =>
        parseStatement(statement, context, direction)
      case expression: IASTExpression =>
        parseExpression(expression, direction, context)
      case array: IASTArrayModifier =>
        arraySize = array.getConstantExpression.getRawSignature.toInt
        Seq()
      case param: IASTParameterDeclaration =>
        if (direction == Exiting) {
          val arg = context.stack.pop
          functionArgumentMap += (param.getDeclarator.getName.getRawSignature -> arg)
          Seq()
        } else {
          Seq(param.getDeclarator)
        }

      case tUnit: IASTTranslationUnit =>
        if (direction == Entering) {
          tUnit.getDeclarations
        } else {
          Seq()
        }
      case simple: IASTSimpleDeclaration =>
        if (direction == Entering) {
          simple.getDeclarators
        } else {
          Seq()
        }
      case fcnDec: IASTFunctionDeclarator =>
        if (direction == Entering) {
          fcnDec.getChildren.filter(x => !x.isInstanceOf[IASTName]).map{x => x}
        } else {
          Seq()
        }
      case decl: IASTDeclarator =>
        parseDeclarator(decl, direction, context)
      case fcnDef: IASTFunctionDefinition =>
        if (isPreprocessing) {
          functionMap += (fcnDef.getDeclarator.getName.getRawSignature -> fcnDef)
          Seq()
        } else if (direction == Exiting) {
          if (!functionReturnStack.isEmpty) {
            // We are exiting a function we're currently executing
            functionArgumentMap.clear
            Seq()
          } else {
            Seq()
          }
        } else {
          Seq(fcnDef.getDeclarator, fcnDef.getBody)
        }
      case decl: IASTSimpleDeclaration =>
        Seq()
      case eq: IASTEqualsInitializer =>
        if (direction == Entering) {
          isVarInitialized = true
          Seq(eq.getInitializerClause)
        } else {
          Seq()
        }
    }
  }

  def parseDeclarator(decl: IASTDeclarator, direction: Direction, context: IASTContext): Seq[IASTNode] = {
    if ((direction == Exiting || direction == Visiting) && !decl.getParent.isInstanceOf[IASTParameterDeclaration]) {
      var value: Any = null // init to zero
      if (isVarInitialized) {
        value = context.stack.pop
      }
      if (arraySize > 0) {
        context.variableMap += (decl.getName.getRawSignature -> Array.fill(arraySize)(0))
      } else {
        //println("ADDING GLOBAL VAR: " + decl.getName.getRawSignature + ", " + value)
        context.variableMap += (decl.getName.getRawSignature -> value)
      }
      Seq()
    } else {
      arraySize = 0
      isVarInitialized = false
      if (decl.getInitializer != null) {
        Seq(decl.getInitializer)
      } else {
        Seq()
      }
    }

  }

  def castLiteral(lit: IASTLiteralExpression): Any = {
    val string = lit.getRawSignature
    if (string.head == '\"' && string.last == '\"') {
      string
    } else if (isLongNumber(string)) {
      string.toInt
    } else {
      string.toDouble
    }
  }

  def parseBinaryOperand(op: IASTExpression, context: IASTContext): Any = {
    op match {
      case lit: IASTLiteralExpression => castLiteral(lit)
      case id: IASTIdExpression => {
        if (context.variableMap.contains(id.getRawSignature)) {
          context.variableMap(id.getRawSignature)
        } else {
          functionArgumentMap(id.getRawSignature)
        }
      }
      case sub: IASTArraySubscriptExpression =>
        context.variableMap(sub.getArrayExpression.getRawSignature).asInstanceOf[Array[_]](sub.getArgument.getRawSignature.toInt)
      case bin: IASTBinaryExpression => context.stack.pop
      case bin: IASTUnaryExpression => context.stack.pop
      case fcn: IASTFunctionCallExpression => context.stack.pop
    }
  }

  def parseBinaryExpr(binaryExpr: IASTBinaryExpression, direction: Direction, context: IASTContext): Any = {
    if (direction == Exiting || direction == Visiting) {

      val op2 = context.stack.pop //parseBinaryOperand(binaryExpr.getOperand1, context)
      val op1 = context.stack.pop //parseBinaryOperand(binaryExpr.getOperand2, context)

      binaryExpr.getOperator match {
        case `op_multiply` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              x * y
            case (x: Double, y: Int) =>
              x * y
            case (x: Int, y: Double) =>
              x * y
            case (x: Double, y: Double) =>
              x * y
          }
        case `op_plus` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              x + y
            case (x: Double, y: Int) =>
              x + y
            case (x: Int, y: Double) =>
              x + y
            case (x: Double, y: Double) =>
              x + y
          }
        case `op_minus` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              x - y
            case (x: Double, y: Int) =>
              x - y
            case (x: Int, y: Double) =>
              x - y
            case (x: Double, y: Double) =>
              x - y
          }
        case `op_divide` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              x / y
            case (x: Double, y: Int) =>
              x / y
            case (x: Int, y: Double) =>
              x / y
            case (x: Double, y: Double) =>
              x / y
          }
        case `op_assign` =>
          println("SETTING: " + binaryExpr.getOperand1.getRawSignature + " to " + op2)
          context.variableMap += (binaryExpr.getOperand1.getRawSignature -> op2)
          null
        case `op_equals` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              x == y
            case (x: Double, y: Int) =>
              x == y
            case (x: Int, y: Double) =>
              x == y
            case (x: Double, y: Double) =>
              x == y
          }
        case _ => throw new Exception("unhandled binary operator"); null
      }
    } else {
      null
    }
  }

  def execute = {

    def runProgram(current: IASTNode, direction: Direction): Unit = {
      //println(current.getClass.getSimpleName)
      // while there is still an execution context to run
      val newPaths = step(current, mainContext, direction)

      newPaths.foreach{ node =>
        runProgram(node, Entering)
        runProgram(node, Exiting)
      }
    }

    runProgram(tUnit, Entering)
    //println("RUNNING PROGRAM")
    isPreprocessing = false
    mainContext.stack.clear
    runProgram(functionMap("main"), Entering)
  }
}
