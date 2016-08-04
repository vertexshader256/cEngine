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
  var currentIndex = 0

  //val globalScope = new Scope(null)
  val path = Utils.getPath(tUnit)

  val integerStack = new Stack[Int]()
  val variableMap = scala.collection.mutable.Map[String, IntPrimitive]()
  val functionMap = scala.collection.mutable.Map[String, Int]()

  var functionReturnStack = new Stack[Int]()

  val functionArgumentMap = scala.collection.mutable.Map[String, Int]()

  var isRunning = false

  def step(current: Path, next: Path, wholePath: Seq[Path]) = {

    val direction = current.direction

    current.node match {
      case param: IASTParameterDeclaration =>
        if (direction == Exiting) {
          functionArgumentMap += (param.getDeclarator.getName.getRawSignature -> integerStack.pop)
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
            functionArgumentMap.clear
          }
        } else if (direction == Entering) {
          functionMap += (fcnDef.getDeclarator.getName.getRawSignature -> currentIndex)

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
              stdout += variableMap(args(1).getRawSignature).value.toString
            }
          } else {
            functionReturnStack.push(currentIndex)
            currentIndex = functionMap(name)

            args.foreach{ arg =>
              integerStack.push(arg.getRawSignature.toInt)
            }
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
            case id: IASTIdExpression => {
              if (variableMap.contains(id.getRawSignature)) {
                variableMap(id.getRawSignature).value.toInt
              } else {
                functionArgumentMap(id.getRawSignature)
              }
            }
            case bin: IASTBinaryExpression => integerStack.pop
            case bin: IASTUnaryExpression => integerStack.pop
          })

          val op2 = binaryExpr.getOperand2 match {
            case lit: IASTLiteralExpression => lit.getRawSignature.toInt
            case id: IASTIdExpression => {
              if (variableMap.contains(id.getRawSignature)) {
                variableMap(id.getRawSignature).value.toInt
              } else {
                functionArgumentMap(id.getRawSignature)
              }
            }
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

  def jumpToExit() = {
    val start = currentPath
    if (start.direction == Entering) {
      while (currentPath.node != start.node || currentPath.direction != Exiting) {
        currentIndex += 1
        currentPath = path(currentIndex)
      }
    } else {
      throw new Exception("Cannot jump if not entering")
    }
  }

  def execute = {
    var isDone = false
    var currentNode = path.head.node

    while (!isDone) {
      if (currentIndex == path.size - 1) {
        step(path(currentIndex), null, path)
        currentIndex += 1
      } else if (currentIndex == path.size) {
        isDone = true
      } else {
        currentPath = path(currentIndex)
        step(path(currentIndex), path(currentIndex + 1), path)
        currentIndex += 1
      }
    }
  }
}
