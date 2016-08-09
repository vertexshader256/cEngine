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

case class Context(startNode: IASTNode) {
  var currentPath: Path = null
  val stack = new Stack[Any]()
  val variableMap = scala.collection.mutable.Map[String, Any]()
  val path = Utils.getPath(startNode)
}

class Executor(code: String) {

  var isPreprocessing = true
  val tUnit = Utils.getTranslationUnit(code)

  val stdout = new ListBuffer[String]()

  val mainContext = new Context(tUnit)

  val functionMap = scala.collection.mutable.Map[String, IASTNode]()

  var functionReturnStack = new Stack[IASTFunctionCallExpression]()
  val functionArgumentMap = scala.collection.mutable.Map[String, Any]()

  var isVarInitialized = false
  var arraySize = 0

  def isLongNumber(s: String): Boolean = (allCatch opt s.toLong).isDefined
  def isDoubleNumber(s: String): Boolean = (allCatch opt s.toDouble).isDefined

  case class IASTPath(node: IASTNode, direction: Direction)

  def printf(context: Context) = {
    var current = context.stack.pop
    val formatString = current.asInstanceOf[String].replaceAll("^\"|\"$", "")


    def getNumericArg() = {
      current = context.stack.pop
//      val result = if (current.isInstanceOf[String]) {
//        arg
//      } else if (current.isInstanceOf[IASTBinaryExpression] || args(currentArg).isInstanceOf[IASTFunctionCallExpression]) {
//        // the argument is an expression
//        context.stack.pop.toString
//      } else {
//        // the argument is just a variable reference
//        context.variableMap(arg).toString
//      }
      current
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

  def parseStatement(statement: IASTStatement, context: Context, direction: Direction): Seq[IASTPath] = statement match {
    case ifStatement: IASTIfStatement =>
      if (direction == Entering) {
        Seq(IASTPath(ifStatement.getConditionExpression, Entering))
      } else {
        val conditionResult = context.stack.pop match {
          case x: Int => x == 1
          case x: Boolean => x
        }
        if (conditionResult) {
          Seq(IASTPath(ifStatement.getThenClause, Entering))
        } else {
          Seq(IASTPath(ifStatement.getElseClause, Entering))
        }
      }
    case ret: IASTReturnStatement =>
      if (direction == Entering) {
        Seq(IASTPath(ret.getReturnValue, Entering))
      } else {
        Seq()
      }
    case decl: IASTDeclarationStatement =>
      if (direction == Entering) {
        Seq(IASTPath(decl.getDeclaration, Entering))
      } else {
        Seq()
      }
    case compound: IASTCompoundStatement =>
      if (direction == Entering) {
        compound.getStatements.map{x => IASTPath(x, Entering)}
      } else {
        Seq()
      }
    case exprStatement: IASTExpressionStatement =>
      if (direction == Entering) {
        Seq(IASTPath(exprStatement.getExpression, Entering))
      } else {
        Seq()
      }
  }

  def parseExpression(expr: IASTExpression, direction: Direction, context: Context): Seq[IASTPath] = expr match {
    case subscript: IASTArraySubscriptExpression =>
      Seq()
    case unary: IASTUnaryExpression =>
      if (direction == Entering) {
        Seq(IASTPath(unary.getOperand, Entering))
      } else {
        Seq()
      }
    case lit: IASTLiteralExpression =>
      //   HACK ALERT
      //   FIX THIS (dont have it specific to IF statements)
      //if (lit.getParent.isInstanceOf[IASTIfStatement]) {
      if (direction == Exiting) {
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
          Seq(IASTPath(functionMap(name), Entering))
        }

      } else {
        call.getArguments.map{x => IASTPath(x, Entering)}
      }

    case bin: IASTBinaryExpression =>
      if (direction == Exiting) {
        val result = parseBinaryExpr(bin, direction, context)
        if (result != null) {
          context.stack.push(result)
        }
        Seq()
      } else {
        Seq(IASTPath(bin.getOperand1, Entering), IASTPath(bin.getOperand2, Entering))
      }
  }

  def step(current: IASTNode, context: Context, direction: Direction): Seq[IASTPath] = {

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
          Seq(IASTPath(param.getDeclarator, Entering))
        }

      case tUnit: IASTTranslationUnit =>
        if (direction == Entering) {
          tUnit.getDeclarations.map{x => IASTPath(x, Entering)}
        } else {
          Seq()
        }
      case simple: IASTSimpleDeclaration =>
        if (direction == Entering) {
          simple.getDeclarators.map{x => IASTPath(x, Entering)}
        } else {
          Seq()
        }
      case fcnDec: IASTFunctionDeclarator =>
        if (direction == Entering) {
          fcnDec.getChildren.filter(x => !x.isInstanceOf[IASTName]).map{x => IASTPath(x, Entering)}
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
            Seq(IASTPath(functionReturnStack.pop, Exiting))
          } else {
            Seq()
          }
        } else {
          Seq(IASTPath(fcnDef.getDeclarator, Entering), IASTPath(fcnDef.getBody, Entering))
        }
      case decl: IASTSimpleDeclaration =>
        Seq()
      case eq: IASTEqualsInitializer =>
        if (direction == Entering) {
          isVarInitialized = true
          Seq(IASTPath(eq.getInitializerClause, Entering))
        } else {
          Seq()
        }
    }
  }

  def parseDeclarator(decl: IASTDeclarator, direction: Direction, context: Context): Seq[IASTPath] = {
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
        Seq(IASTPath(decl.getInitializer, Entering))
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

  def parseBinaryOperand(op: IASTExpression, context: Context): Any = {
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

  def parseBinaryExpr(binaryExpr: IASTBinaryExpression, direction: Direction, context: Context): Any = {
    if (direction == Exiting || direction == Visiting) {

      val op1 = context.stack.pop //parseBinaryOperand(binaryExpr.getOperand1, context)
      val op2 = context.stack.pop //parseBinaryOperand(binaryExpr.getOperand2, context)

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
      val forwardPaths = newPaths.filter{x => x.direction == Entering}

      if (!newPaths.isEmpty) {
        forwardPaths.foreach { case IASTPath(node, dir) => runProgram(node, Entering) }
        forwardPaths.reverse.foreach { case IASTPath(node, dir) => runProgram(node, Exiting) }
      }
    }

    runProgram(tUnit, Entering)
    //println("RUNNING PROGRAM")
    isPreprocessing = false
    mainContext.stack.clear
    runProgram(functionMap("main"), Entering)
  }
}
