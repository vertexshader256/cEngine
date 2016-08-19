package scala.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;

case class IASTContext(startNode: IASTNode) {
  val stack = new Stack[Any]()
  val variables = new ListBuffer[Var]()
  val path = Utils.getPath(startNode)
  val visitedStack = new Stack[Visited]()
  var currentVisited: Visited = null
  var functionReturnStack = new Stack[IASTFunctionCallExpression]()
  val functionMap = scala.collection.mutable.Map[String, IASTNode]()
  val stdout = new ListBuffer[String]()
  
  def doesVariableExist(name: String): Boolean = {
    variables.exists(_.name == name)
  }
  
  def getVariable(name: String): Var = {
    variables.find(_.name == name).get
  }
  
  def callFunction(call: IASTFunctionCallExpression) = {
    visitedStack.push(currentVisited)
    currentVisited = Visited(new ListBuffer[IASTNode](), new ListBuffer[Var]())
    functionReturnStack.push(call)
    
    val name = call.getFunctionNameExpression match {
      case x: IASTIdExpression => x.getName.getRawSignature
      case _ => "Error"
    }
    
    Seq(functionMap(name))
  }
  
  def resolveId(id: String) = {
    if (doesVariableExist(id)) {
      getVariable(id)
    } else {
      currentVisited.getArg(id)
    }
  }
  
  def clearVisited(parent: IASTNode) {
    currentVisited.nodes -= parent
    parent.getChildren.foreach { node =>
      clearVisited(node)
    }
  }
}

trait Var {
  def name: String
  var value: Any
  
  def sizeof: Int = value match {
    case Pointer(_,_) => 4
    case array: Array[Variable] => array.length * array.head.sizeof
    case _: Int => 4
    case _: Double => 8
    case _: Float => 4
    case _: Boolean => 4
    case _: Char => 1
  }
}

case class Variable(val name: String, var value: Any) extends Var
case class Pointer(val name: String, var value: Any) extends Var

case class VarRef(name: String) extends AnyVal

case class Visited(nodes: ListBuffer[IASTNode], functionArgs: ListBuffer[Var]) {
  def getArg(name: String): Var = {
    functionArgs.find(_.name == name).get
  }
}

class Executor(code: String) {

  var isPreprocessing = true
  val tUnit = Utils.getTranslationUnit(code)

  

  val mainContext = new IASTContext(tUnit)

  
  
  var isArrayDeclaration = false
  
  
  
  var currentType: IASTDeclSpecifier = null

  

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
          context.clearVisited(forLoop.getBody)
          context.clearVisited(forLoop.getIterationExpression)
          context.clearVisited(forLoop.getConditionExpression)
          
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

  

  def step(current: IASTNode, context: IASTContext, direction: Direction): Seq[IASTNode] = {

    current match {
      case statement: IASTStatement =>
        parseStatement(statement, context, direction)
      case expression: IASTExpression =>
        Expression.parse(expression, direction, context)
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
          context.currentVisited.functionArgs += Variable(param.getDeclarator.getName.getRawSignature, arg)
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
          currentType = simple.getDeclSpecifier
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
          context.functionMap += (fcnDef.getDeclarator.getName.getRawSignature -> fcnDef)
          Seq()
        } else if (direction == Exiting) {
          context.currentVisited = context.visitedStack.pop
          if (!context.functionReturnStack.isEmpty) {
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
      case initList: IASTInitializerList =>
        if (direction == Entering) {
          initList.getClauses
        } else {
          Seq()
        }
      case typeId: IASTTypeId =>
        if (direction == Exiting) {
           context.stack.push(typeId.getDeclSpecifier.getRawSignature)
        }
        Seq()
      case spec: IASTSimpleDeclSpecifier =>
         println("PUSHING TYPE SIG")
        if (direction == Entering) {
          Seq()
        } else {
         
          context.stack.push(spec.getRawSignature)
          Seq()
        }
    }
  }

  def parseDeclarator(decl: IASTDeclarator, direction: Direction, context: IASTContext): Seq[IASTNode] = {
    if (direction == Exiting) {
      context.stack.push(decl.getName.getRawSignature)
      
      val typeName = currentType.getRawSignature
      
      val initial = typeName match {
          case "int" => 0.toInt
          case "double" => 0.0.toDouble
          case "char" => 0.toChar
          case _ => throw new Exception("No match for " + typeName)
      }
      
      if (isArrayDeclaration) {
        
        val name = context.stack.pop.asInstanceOf[String]
        
        val size = context.stack.pop.asInstanceOf[Int]
       
        val initialArray = Array.fill(size)(new Variable("", initial))
        
        if (!context.stack.isEmpty) { 
          var i = 0
          
          for (i <- (size - 1) to 0 by -1) {
            initialArray(i).value = context.stack.pop
          }
        }
        
        context.variables += new Variable(name, initialArray)
      } else {   
        
        val name = context.stack.pop.asInstanceOf[String]
        
        if (!decl.getPointerOperators.isEmpty) {
          if (!context.stack.isEmpty) {
            // initial value is on the stack, set it
            context.variables += Pointer(name, context.stack.pop.asInstanceOf[Variable])
          } else {
            context.variables += Pointer(name, null)
          }
        } else {
          if (!context.stack.isEmpty) {
            // initial value is on the stack, set it
            context.variables += new Variable(name, context.stack.pop)
          } else {
            context.variables += new Variable(name, initial)
          }
        }
      }
      
      Seq()
    } else {
      isArrayDeclaration = false

      decl match {
        case array: IASTArrayDeclarator =>
          Seq(Option(decl.getInitializer)).flatten ++ array.getArrayModifiers
        case _ =>
          Seq(Option(decl.getInitializer)).flatten
      }
    }
  }

  def execute = {
    
    val pathStack = new Stack[IASTNode]()
  
    var current: IASTNode = null

    def tick(): Unit = {
      val direction = if (mainContext.currentVisited.nodes.contains(current)) Exiting else Entering
      
      //println("BEGIN: " + current.getClass.getSimpleName + ":" + direction)   
      
      val paths: Seq[IASTNode] = step(current, mainContext, direction)        
      
      if (direction == Exiting) {
        pathStack.pop
      } else {
        mainContext.currentVisited.nodes += current
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
    
    mainContext.visitedStack.push(Visited(new ListBuffer[IASTNode](), new ListBuffer[Var]())) // load initial stack
    mainContext.currentVisited = mainContext.visitedStack.head

    runProgram()
    isPreprocessing = false
    mainContext.stack.clear
    
    println("_----------------------------------------------_")
    
    mainContext.visitedStack.clear
    mainContext.visitedStack.push(Visited(new ListBuffer[IASTNode](), new ListBuffer[Var]())) // load initial stack
    mainContext.currentVisited = mainContext.visitedStack.head
    pathStack.clear
    pathStack.push(mainContext.functionMap("main"))
    current = pathStack.head

    runProgram()
  }
}
