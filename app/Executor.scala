package scala.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch

case class IASTContext(startNode: IASTNode) {
  val stack = new Stack[Any]()
  val variables = new ListBuffer[Variable]()
  val path = Utils.getPath(startNode)
  
  def doesVariableExist(name: String): Boolean = {
    variables.exists(_.name == name)
  }
  
  def getVariable(name: String): Variable = {
    variables.find(_.name == name).get
  }
}

case class Variable(name: String, var value: Any)
case class VarRef(name: String) extends AnyVal

case class Visited(nodes: ListBuffer[IASTNode], functionArgs: scala.collection.mutable.Map[String, Any])

class Executor(code: String) {

  var isPreprocessing = true
  val tUnit = Utils.getTranslationUnit(code)

  val stdout = new ListBuffer[String]()

  val mainContext = new IASTContext(tUnit)

  val functionMap = scala.collection.mutable.Map[String, IASTNode]()
  var functionReturnStack = new Stack[IASTFunctionCallExpression]()
  var isArrayDeclaration = false
  
  val visitedStack = new Stack[Visited]()
  var currentVisited: Visited = null

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
        val result = context.stack.pop
        
        val value = result match {
          case VarRef(name) =>
            context.getVariable(name).value
          case x => x
        }

        val conditionResult = value match {
          case x: Int => x == 1
          case x: Boolean => x
        }
        if (conditionResult) {
          Seq(ifStatement.getThenClause)
        } else {
          Seq(ifStatement.getElseClause)
        }
      }
    case forLoop: IASTForStatement =>
      if (direction == Entering) {
        Seq(forLoop.getInitializerStatement, forLoop.getConditionExpression)
      } else {
        val shouldKeepLooping = context.stack.pop.asInstanceOf[Boolean]
      
        if (shouldKeepLooping) {
          clearVisited(forLoop.getBody)
          clearVisited(forLoop.getIterationExpression)
          clearVisited(forLoop.getConditionExpression)
          
          Seq(forLoop.getBody, forLoop.getIterationExpression, forLoop.getConditionExpression, forLoop)
        } else {
          Seq()
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
      if (direction == Entering) {
        Seq(subscript.getArrayExpression, subscript.getArgument)
      } else {
          val inputs = (context.stack.pop, context.stack.pop)
        
          inputs match {
            case (VarRef(indexVarName), VarRef(name)) => 
               val index = context.getVariable(indexVarName).value.asInstanceOf[Int]
               val arrayValue = context.getVariable(name).value.asInstanceOf[Array[Variable]](index)
               context.stack.push(arrayValue)
            case (index: Int, VarRef(name)) => 
              val arrayValue = context.getVariable(name).value.asInstanceOf[Array[Variable]](index)
              context.stack.push(arrayValue)
          }

          Seq()
      }
    case unary: IASTUnaryExpression =>
      import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression._
      
      if (direction == Entering) {
        Seq(unary.getOperand)
      } else {
        unary.getOperator match {
          case `op_postFixIncr` => {           
            context.stack.pop match {
              case VarRef(name) =>
                context.getVariable(name).value = context.getVariable(name).value.asInstanceOf[Int] + 1
            }
          }
          case `op_postFixDecr` => {
            context.stack.pop match {
              case VarRef(name) =>
                context.getVariable(name).value = context.getVariable(name).value.asInstanceOf[Int] - 1
            }
          }
          case `op_bracketedPrimary` => // not sure what this is for
        }
        Seq()
      }
    case lit: IASTLiteralExpression =>
      if (direction == Exiting) {
        println("PUSHING LIT: " + castLiteral(lit))
        context.stack.push(castLiteral(lit))
      }
      Seq()
    case id: IASTIdExpression =>
      if (direction == Exiting) {
        println("PUSHING ID: " + id.getName.getRawSignature)
        context.stack.push(VarRef(id.getName.getRawSignature))
      }
      Seq()
    case call: IASTFunctionCallExpression =>
      // only evaluate after leaving
      if (direction == Exiting) {
        val name = call.getFunctionNameExpression match {
          case x: IASTIdExpression => x.getName.getRawSignature
          case _ => "Error"
        }
        
        val argList = call.getArguments.map { arg => (arg, context.stack.pop) }
        
        val resolved = argList.map { case (arg, value) => 
          value match {
            case VarRef(name) =>
               context.getVariable(name).value
            case Variable(_, value) =>
               value
            case _ => value
          }
        }
        
        resolved.reverse.foreach { arg => context.stack.push(arg)}

        if (name == "printf") {
          printf(context)
          Seq()
        } else {
          visitedStack.push(currentVisited)
          currentVisited = Visited(new ListBuffer[IASTNode](), scala.collection.mutable.Map[String, Any]())
          println("GOING TO FUNCTIONNNNNNNNNNNNNNNNNNNN")
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
          println("RETURNING FROM BIN EXPR: " + result)
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
        if (direction == Exiting) {
          isArrayDeclaration = true
          Seq()
        } else {
          Seq(array.getConstantExpression)
        }

      case param: IASTParameterDeclaration =>
        if (direction == Exiting) {
          val arg = context.stack.pop
          println("SETTING " + param.getDeclarator.getName.getRawSignature + " to " + arg)
          currentVisited.functionArgs += (param.getDeclarator.getName.getRawSignature -> arg)
          Seq()
        } else {
          Seq()
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
          currentVisited = visitedStack.pop
          if (!functionReturnStack.isEmpty) {
            // We are exiting a function we're currently executing
            //functionArgumentMap.clear
            Seq()
          } else {
            Seq()
          }
        } else {
          
          Seq(fcnDef.getDeclarator, fcnDef.getBody)
        }
      case eq: IASTEqualsInitializer =>
        if (direction == Entering) {
          Seq(eq.getInitializerClause)
        } else {
          Seq()
        }
    }
  }

  def parseDeclarator(decl: IASTDeclarator, direction: Direction, context: IASTContext): Seq[IASTNode] = {
    if (direction == Exiting) {
      
      if (isArrayDeclaration) {
        val size = context.stack.pop.asInstanceOf[Int]
        context.variables += Variable(decl.getName.getRawSignature, Array.fill(size)(Variable("", 0)))
      } else if (!context.stack.isEmpty) {
        // initial value is on the stack, set it
        context.variables += Variable(decl.getName.getRawSignature, context.stack.pop)
      } else {
        context.variables += Variable(decl.getName.getRawSignature, 0)
      }
      Seq()
    } else {
      isArrayDeclaration = false

      decl match {
        case array: IASTArrayDeclarator =>
          array.getArrayModifiers
        case _ =>
          Seq(Option(decl.getInitializer)).flatten
      }
    }
  }

  def castLiteral(lit: IASTLiteralExpression): Any = {
    
    def isLongNumber(s: String): Boolean = (allCatch opt s.toLong).isDefined
    def isDoubleNumber(s: String): Boolean = (allCatch opt s.toDouble).isDefined
    
    val string = lit.getRawSignature
    if (string.head == '\"' && string.last == '\"') {
      string
    } else if (isLongNumber(string)) {
      string.toInt
    } else {
      string.toDouble
    }
  }

  def parseBinaryExpr(binaryExpr: IASTBinaryExpression, direction: Direction, context: IASTContext): Any = {
    import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._
    
    if (direction == Exiting) {

      var op2: Any = context.stack.pop
      var op1: Any = context.stack.pop
      
      def resolveOp1() = op1 = op1 match {
        case VarRef(name) => 
          if (context.doesVariableExist(name)) {
            context.getVariable(name).value
          } else {
            currentVisited.functionArgs(name)
          }
        case Variable(_, value) => value
        case int: Int => int
        case bool: Boolean => bool
        case double: Double => double
      }
      
      def resolveOp2() = op2 = op2 match {
        case VarRef(name) => 
          if (context.doesVariableExist(name)) {
            context.getVariable(name).value
          } else {
            currentVisited.functionArgs(name)
          }
        case Variable(_, value) => value
        case int: Int => int
        case bool: Boolean => bool
        case double: Double => double
      }

      binaryExpr.getOperator match {
        case `op_multiply` =>
          resolveOp1()
          resolveOp2()
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
          resolveOp1()
          resolveOp2()
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
          resolveOp1()
          resolveOp2()
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
          resolveOp1()
          resolveOp2()
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
          
          resolveOp2()
          
          op1 match {
            case variable: Variable =>
              variable.value = op2
            case VarRef(name) =>
              context.getVariable(name).value = op2
          }

          null
        case `op_equals` =>
          resolveOp1()
          resolveOp2()
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
        case `op_greaterThan` =>
          resolveOp1()
          resolveOp2()
          (op1, op2) match {
            case (x: Int, y: Int) =>
              x > y
            case (x: Double, y: Int) =>
              x > y
            case (x: Int, y: Double) =>
              x > y
            case (x: Double, y: Double) =>
              x > y
          }
        case `op_lessThan` =>
          resolveOp1()
          resolveOp2()
          (op1, op2) match {
            case (x: Int, y: Int) =>
              x < y
            case (x: Double, y: Int) =>
              x < y
            case (x: Int, y: Double) =>
              x < y
            case (x: Double, y: Double) =>
              x < y
          }
        case `op_plusAssign` =>
          resolveOp2()
          op1 match {
            case VarRef(name) => context.getVariable(name).value = context.getVariable(name).value.asInstanceOf[Int] + op2.asInstanceOf[Int]
          }
        case `op_minusAssign` =>
          resolveOp2()
          op1 match {
            case VarRef(name) => context.getVariable(name).value = context.getVariable(name).value.asInstanceOf[Int] - op2.asInstanceOf[Int]
          }
        case `op_logicalAnd` =>
          resolveOp1()
          resolveOp2()
          (op1, op2) match {
            case (x: Boolean, y: Boolean) =>
              x && y
          }
        case `op_logicalOr` =>
          resolveOp1()
          resolveOp2()
          (op1, op2) match {
            case (x: Boolean, y: Boolean) =>
              x || y
          }
        case _ => throw new Exception("unhandled binary operator"); null
      }
    } else {
      null
    }
  }
  
  def clearVisited(parent: IASTNode) {
        currentVisited.nodes -= parent
        parent.getChildren.foreach { node =>
          clearVisited(node)
        }
    }

  def execute = {
    
    val pathStack = new Stack[IASTNode]()
    
  
    var current: IASTNode = null

    def tick(): Unit = {
      val direction = if (currentVisited.nodes.contains(current)) Exiting else Entering
      
      println("BEGIN: " + current.getClass.getSimpleName + ":" + direction)   
      
      val paths: Seq[IASTNode] = step(current, mainContext, direction)        
      
      if (direction == Exiting) {
        pathStack.pop
      } else {
        currentVisited.nodes += current
      }
      
      paths.reverse.foreach{path => pathStack.push(path)}
      
      if (!pathStack.isEmpty) {
        current = pathStack.head
      } else {
        current = null
      }
    }
    
    def runProgram() = {
      while (current != null) {
        tick()
      }
    }
    
    current = tUnit
    
    visitedStack.push(Visited(new ListBuffer[IASTNode](), scala.collection.mutable.Map[String, Any]())) // load initial stack
    currentVisited = visitedStack.head

    runProgram()
    isPreprocessing = false
    mainContext.stack.clear
    
    println("_----------------------------------------------_")
    
    visitedStack.clear
    visitedStack.push(Visited(new ListBuffer[IASTNode](), scala.collection.mutable.Map[String, Any]())) // load initial stack
    currentVisited = visitedStack.head
    pathStack.clear
    pathStack.push(functionMap("main"))
    current = pathStack.head

    runProgram()
  }
}
