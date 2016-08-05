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

class Executor(code: String) {

  val tUnit = Utils.getTranslationUnit(code)

  val stdout = new ListBuffer[String]()
  var currentScope: IScope = tUnit.getScope
  var currentNode: IASTNode = null
  var currentPath: Path = null

  //val globalScope = new Scope(null)
  val path = Utils.getPath(tUnit)

  val stack = new Stack[Any]()
  val variableMap = scala.collection.mutable.Map[String, Any]()
  val functionMap = scala.collection.mutable.Map[String, Path]()

  var functionReturnStack = new Stack[Path]()

  val functionArgumentMap = scala.collection.mutable.Map[String, Any]()

  var isRunning = false
  var isDone = false
  var isVarInitialized = false
  var arraySize = 0

  def isLongNumber(s: String): Boolean = (allCatch opt s.toLong).isDefined

  def isDoubleNumber(s: String): Boolean = (allCatch opt s.toDouble).isDefined

  def prestep(current: Path, next: Path, wholePath: Seq[Path]): Unit = {

    val direction = current.direction

    current.node match {
      case array: IASTArrayModifier =>
      case fcnDef: IASTFunctionDefinition =>
        if (direction == Entering) {
          functionMap += (fcnDef.getDeclarator.getName.getRawSignature -> currentPath)
          jumpToExit()
        }
      case fcnDecl: IASTFunctionDeclarator =>
      case decl: IASTDeclarator =>
        if (direction == Exiting || direction == Visiting) {
          var value: Any = null // init to zero
          if (isVarInitialized) {
            value = stack.pop
          }
          //println("ADDING GLOBAL VAR: " + decl.getName.getRawSignature + ", " + value)
          variableMap += (decl.getName.getRawSignature -> value)
        } else {
          isVarInitialized = false
        }
      case equalsInit: IASTEqualsInitializer =>
        isVarInitialized = true

        equalsInit.getInitializerClause match {
          case lit: IASTLiteralExpression =>
            if (isLongNumber(lit.getRawSignature)) {
              stack.push(lit.getRawSignature.toInt)
            } else {
              stack.push(lit.getRawSignature.toDouble)
            }
          case _ => // dont do anything
        }
      case bin: IASTBinaryExpression =>
        parseBinaryExpr(bin, direction)
      case _ =>
    }
  }

  def step(current: Path, next: Path, wholePath: Seq[Path]) = {

    val direction = current.direction

    current.node match {
      case subscript: IASTArraySubscriptExpression =>
      case array: IASTArrayModifier =>
        arraySize = array.getConstantExpression.getRawSignature.toInt
        println("MOD")
      case param: IASTParameterDeclaration =>
        if (direction == Exiting) {
          val arg = stack.pop
          functionArgumentMap += (param.getDeclarator.getName.getRawSignature -> arg)
        }
      case unary: IASTUnaryExpression =>
      case id: IASTIdExpression =>
      case tUnit: IASTTranslationUnit =>
      case simple: IASTSimpleDeclaration =>
      case fcnDec: IASTFunctionDeclarator =>
      case ret: IASTReturnStatement =>
        ret.getReturnValue match {
          case lit: IASTLiteralExpression =>
            stack.push(lit.getRawSignature.toInt)
          case _ =>
        }
      case decl: IASTDeclarator =>
        if ((direction == Exiting || direction == Visiting) && !decl.getParent.isInstanceOf[IASTParameterDeclaration]) {
          var value: Any = null // init to zero
          if (isVarInitialized) {
            value = stack.pop
          }
          if (arraySize > 0) {
            variableMap += (decl.getName.getRawSignature -> Array.fill(arraySize)(0))
          } else {
            //println("ADDING GLOBAL VAR: " + decl.getName.getRawSignature + ", " + value)
            variableMap += (decl.getName.getRawSignature -> value)
          }
        } else {
          arraySize = 0
          isVarInitialized = false
        }
      case fcnDef: IASTFunctionDefinition =>
        if (direction == Exiting) {
          if (fcnDef.getDeclarator.getName.getRawSignature == "main") {
            isDone = true
          } else if (!functionReturnStack.isEmpty) {
            // We are exiting a function we're currently executing
            currentPath = functionReturnStack.pop
            functionArgumentMap.clear
          }
        }
        else if (direction == Entering) {
          //functionMap += (fcnDef.getDeclarator.getName.getRawSignature -> currentIndex)

          if (fcnDef.getDeclarator.getName.getRawSignature != "main") { // don't skip the main function
            jumpToExit()
          }
        }
      case decl: IASTSimpleDeclaration =>
      case call: IASTFunctionCallExpression =>

        // only evaluate after leaving
        if (direction == Exiting) {
          val name = call.getFunctionNameExpression match {
            case x: IASTIdExpression => x.getName.getRawSignature
            case _ => "Error"
          }
          val args = call.getArguments

          if (name == "printf") {
            val secondArg = args(1).getRawSignature
            if (secondArg.head == '\"' || secondArg.last == '\"') {
              stdout += args(1).getRawSignature.tail.reverse.tail.reverse
            } else if (args(1).isInstanceOf[IASTBinaryExpression] || args(1).isInstanceOf[IASTFunctionCallExpression]) {
              // the argument is an expression
              stdout += stack.pop.toString
            } else {
              // the argument is just a variable reference
              stdout += variableMap(args(1).getRawSignature).toString
            }
          } else {
            functionReturnStack.push(currentPath)
            currentPath = functionMap(name)

            args.foreach{ arg =>
              arg match {
                case x: IASTLiteralExpression =>
                  stack.push(arg.getRawSignature.toInt)
                case _ =>
              }

            }
          }
        }
      case lit: IASTLiteralExpression =>
      case decl: IASTDeclarationStatement =>
      case compound: IASTCompoundStatement =>
      case exprStatement: IASTExpressionStatement =>
      case equalsInit: IASTEqualsInitializer =>
        isVarInitialized = true
        equalsInit.getInitializerClause match {
          case lit: IASTLiteralExpression =>
            if (isLongNumber(lit.getRawSignature)) {
              stack.push(lit.getRawSignature.toInt)
            } else {
              stack.push(lit.getRawSignature.toDouble)
            }
          case _ => // dont do anything
        }
      case bin: IASTBinaryExpression =>
        parseBinaryExpr(bin, direction)

    }
  }

  def parseBinaryOperand(op: IASTExpression): Any = {
    op match {
      case lit: IASTLiteralExpression => lit.getRawSignature.toInt
      case id: IASTIdExpression => {
        if (variableMap.contains(id.getRawSignature)) {
          variableMap(id.getRawSignature)
        } else {
          functionArgumentMap(id.getRawSignature)
        }
      }
      case sub: IASTArraySubscriptExpression =>
        variableMap(sub.getArrayExpression.getRawSignature).asInstanceOf[Array[_]](sub.getArgument.getRawSignature.toInt)
      case bin: IASTBinaryExpression => stack.pop
      case bin: IASTUnaryExpression => stack.pop
      case fcn: IASTFunctionCallExpression => stack.pop
    }
  }

  def parseBinaryExpr(binaryExpr: IASTBinaryExpression, direction: Direction) = {
    if (direction == Exiting || direction == Visiting) {

      val op1 = parseBinaryOperand(binaryExpr.getOperand1)
      val op2 = parseBinaryOperand(binaryExpr.getOperand2)

      binaryExpr.getOperator match {
        case `op_multiply` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              stack.push(x * y)
            case (x: Double, y: Int) =>
              stack.push(x * y)
            case (x: Int, y: Double) =>
              stack.push(x * y)
            case (x: Double, y: Double) =>
              stack.push(x * y)
          }
        case `op_plus` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              stack.push(x + y)
            case (x: Double, y: Int) =>
              stack.push(x + y)
            case (x: Int, y: Double) =>
              stack.push(x + y)
            case (x: Double, y: Double) =>
              stack.push(x + y)
          }
        case `op_minus` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              stack.push(x - y)
            case (x: Double, y: Int) =>
              stack.push(x - y)
            case (x: Int, y: Double) =>
              stack.push(x - y)
            case (x: Double, y: Double) =>
              stack.push(x - y)
          }
        case `op_divide` =>
          (op1, op2) match {
            case (x: Int, y: Int) =>
              stack.push(x / y)
            case (x: Double, y: Int) =>
              stack.push(x / y)
            case (x: Int, y: Double) =>
              stack.push(x / y)
            case (x: Double, y: Double) =>
              stack.push(x / y)
          }
        case `op_assign` =>
          variableMap += (binaryExpr.getOperand1.getRawSignature -> op2)
      }
    }
  }

  def jumpToExit() = {
    val start = currentPath
    if (start.direction == Entering) {
      while (currentPath.node != start.node || currentPath.direction != Exiting) {
        currentPath = path(currentPath.index + 1)
      }
    } else {
      throw new Exception("Cannot jump if not entering")
    }
  }

  def execute = {

    var isDonePreprocessing = false
    currentPath = path.head

    while (!isDonePreprocessing) {
      if (currentPath.index == path.size - 1) {
        isDonePreprocessing = true
      } else {
        prestep(currentPath, path(currentPath.index + 1), path)
        currentPath = path(currentPath.index + 1)
      }
    }

    currentPath = functionMap("main") // start from main
    stack.clear

    while (!isDone) {
        step(currentPath, path(currentPath.index + 1), path)
        currentPath = path(currentPath.index + 1)
    }
  }
}
