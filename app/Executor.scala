package scala.astViewer

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._
import org.eclipse.cdt.core.dom.ast.{IASTEqualsInitializer, _}

import scala.astViewer.{IntPrimitive, Path, Utils}
import scala.collection.mutable.{ListBuffer, Stack}

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

  val integerStack = new Stack[Int]()
  val variableMap = scala.collection.mutable.Map[String, Any]()
  val functionMap = scala.collection.mutable.Map[String, Path]()

  var functionReturnStack = new Stack[Path]()

  val functionArgumentMap = scala.collection.mutable.Map[String, Int]()

  var isRunning = false
  var isDone = false
  var isVarInitialized = false
  var arraySize = 0

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
          var value = 0 // init to zero
          if (isVarInitialized) {
            value = integerStack.pop
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
            integerStack.push(lit.getRawSignature.toInt)
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
          val arg = integerStack.pop
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
            integerStack.push(lit.getRawSignature.toInt)
          case _ =>
        }
      case decl: IASTDeclarator =>
        if ((direction == Exiting || direction == Visiting) && !decl.getParent.isInstanceOf[IASTParameterDeclaration]) {
          var value = 0 // init to zero
          if (isVarInitialized) {
            value = integerStack.pop
          }
          if (arraySize > 0) {
            println("adding array")
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
              stdout += integerStack.pop.toString
            } else {
              // the argument is just a variable reference
              stdout += variableMap(args(1).getRawSignature).asInstanceOf[Int].toString
            }
          } else {
            functionReturnStack.push(currentPath)
            currentPath = functionMap(name)

            args.foreach{ arg =>
              arg match {
                case x: IASTLiteralExpression =>
                  integerStack.push(arg.getRawSignature.toInt)
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
            integerStack.push(lit.getRawSignature.toInt)
          case _ => // dont do anything
        }
      case bin: IASTBinaryExpression =>
        parseBinaryExpr(bin, direction)

    }
  }

  def parseBinaryOperand(op: IASTExpression) = {
    op match {
      case lit: IASTLiteralExpression => lit.getRawSignature.toInt
      case id: IASTIdExpression => {
        if (variableMap.contains(id.getRawSignature)) {
          variableMap(id.getRawSignature).asInstanceOf[Int]
        } else {
          functionArgumentMap(id.getRawSignature)
        }
      }
      case sub: IASTArraySubscriptExpression =>
        variableMap(sub.getArrayExpression.getRawSignature).asInstanceOf[Array[_]](sub.getArgument.getRawSignature.toInt).asInstanceOf[Int]
      case bin: IASTBinaryExpression => integerStack.pop
      case bin: IASTUnaryExpression => integerStack.pop
      case fcn: IASTFunctionCallExpression => integerStack.pop
    }
  }

  def parseBinaryExpr(binaryExpr: IASTBinaryExpression, direction: Direction) = {
    if (direction == Exiting || direction == Visiting) {

      val op1 = parseBinaryOperand(binaryExpr.getOperand1)
      val op2 = parseBinaryOperand(binaryExpr.getOperand2)

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
    integerStack.clear

    while (!isDone) {
        step(currentPath, path(currentPath.index + 1), path)
        currentPath = path(currentPath.index + 1)
    }
  }
}
