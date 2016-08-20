package app.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;

case class IASTContext(startNode: IASTNode) {
  val stack = new Stack[Any]()
  val visitedStack = new Stack[Visited]()
  var vars: Visited = null
  val functionMap = scala.collection.mutable.Map[String, IASTNode]()
  val stdout = new ListBuffer[String]()
  var currentType: IASTDeclSpecifier = null
  
  def callFunction(call: IASTFunctionCallExpression) = {
    visitedStack.push(vars)
    vars = Visited(new ListBuffer[IASTNode](), new ListBuffer[Var](), vars.variables.clone)
    
    val name = call.getFunctionNameExpression match {
      case x: IASTIdExpression => x.getName.getRawSignature
      case _ => "Error"
    }
    
    Seq(functionMap(name))
  }
  
  
  
  def clearVisited(parent: IASTNode) {
    vars.nodes -= parent
    parent.getChildren.foreach { node =>
      clearVisited(node)
    }
  }
}

object TypeHelper {
  val sizeof = new PartialFunction[String, Int] {
    def apply(typeName: String) = typeName match {
        case "int" => 4
        case "double" => 8
        case "float" => 4
        case "bool" => 4
        case "char" => 1
      }
    def isDefinedAt(typeName: String) = Seq("int", "double", "float", "bool", "char").contains(typeName)
  }
}

trait Var {
  def name: String
  var value: Any
  val typeName: String
  
  def sizeof: Int = value match {
    case Pointer(_,_,_) => 4
    case array: Array[Variable] => array.length * array.head.sizeof
    case _ => 
      TypeHelper.sizeof(typeName)
  }
}

case class Variable(val name: String, var value: Any, val typeName: String) extends Var
case class Pointer(val name: String, var value: Any, val typeName: String) extends Var

case class VarRef(name: String) extends AnyVal

case class Visited(nodes: ListBuffer[IASTNode], functionArgs: ListBuffer[Var], variables: ListBuffer[Var]) {
  def getArg(name: String): Var = {
    functionArgs.find(_.name == name).get
  }
  
  def resolveId(id: String) = {
    if (variables.exists(_.name == id)) {
      variables.find(_.name == id).get
    } else {
      getArg(id)
    }
  }
}

class Executor(code: String) {
  var isPreprocessing = true
  val tUnit = Utils.getTranslationUnit(code)

  val mainContext = new IASTContext(tUnit)
  
  var isArrayDeclaration = false
  var isBreaking = false;

  def parseStatement(statement: IASTStatement, context: IASTContext, direction: Direction): Seq[IASTNode] = statement match {
    case breakStatement: IASTBreakStatement =>
      isBreaking = true
      Seq()
    case doWhileLoop: IASTDoStatement =>
      if (direction == Entering) {
        Seq(doWhileLoop.getBody, doWhileLoop.getCondition)
      } else {
        val shouldLoop = context.stack.pop match {
          case x: Int => x == 1
          case x: Boolean => x
        }
      
        if (shouldLoop) {
          context.clearVisited(doWhileLoop.getBody)
          context.clearVisited(doWhileLoop.getCondition)
          
          Seq(doWhileLoop.getBody, doWhileLoop.getCondition, doWhileLoop)
        } else {
          Seq()
        }
      }
    case whileLoop: IASTWhileStatement =>
      if (direction == Entering) {
        Seq(whileLoop.getCondition)
      } else {
        val shouldLoop = context.stack.pop match {
          case x: Int => x == 1
          case x: Boolean => x
        }
      
        if (shouldLoop) {
          context.clearVisited(whileLoop.getBody)
          context.clearVisited(whileLoop.getCondition)
          
          Seq(whileLoop.getBody, whileLoop.getCondition, whileLoop)
        } else {
          Seq()
        }
      }
    case ifStatement: IASTIfStatement =>
      if (direction == Entering) {
        Seq(ifStatement.getConditionExpression)
      } else {
        val result = context.stack.pop
        
        val value = result match {
          case VarRef(name) =>
            context.vars.resolveId(name).value
          case x => x
        }

        val conditionResult = value match {
          case x: Int => x == 1
          case x: Boolean => x
        }
        if (conditionResult) {
          Seq(ifStatement.getThenClause)
        } else if (ifStatement.getElseClause != null) {
          Seq(ifStatement.getElseClause)
        } else {
          Seq()
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
        // resolve everything before returning
        val returnVal = context.stack.pop
        context.stack.push(returnVal match {
          case VarRef(id) => context.vars.resolveId(id).value
          case int: Int => int
          case doub: Double => doub
        })
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
        Expressions.parse(expression, direction, context)
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
          context.vars.functionArgs += Variable(param.getDeclarator.getName.getRawSignature, arg, param.getDeclSpecifier.getRawSignature)
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
          context.currentType = simple.getDeclSpecifier
          simple.getDeclarators
        } else {
          context.currentType = null
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
          context.vars = context.visitedStack.pop
          Seq()
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
      
      val typeName = context.currentType.getRawSignature
      
      val initial = typeName match {
          case "int" => 0.toInt
          case "double" => 0.0.toDouble
          case "char" => 0.toChar
          case _ => throw new Exception("No match for " + typeName)
      }
      
      if (isArrayDeclaration) {
        
        val name = context.stack.pop.asInstanceOf[String]
        
        val size = context.stack.pop.asInstanceOf[Int]
       
        val initialArray = Array.fill(size)(new Variable("", initial, typeName))
        
        if (!context.stack.isEmpty) { 
          var i = 0
          
          for (i <- (size - 1) to 0 by -1) {
            initialArray(i).value = context.stack.pop
          }
        }
        
        context.vars.variables += new Variable(name, initialArray, typeName)
      } else {   
        
        val name = context.stack.pop.asInstanceOf[String]
        
        if (!decl.getPointerOperators.isEmpty) {
          if (!context.stack.isEmpty) {
            // initial value is on the stack, set it
            context.vars.variables += Pointer(name, context.stack.pop.asInstanceOf[Variable], typeName)
          } else {
            context.vars.variables += Pointer(name, null, typeName)
          }
        } else {
          if (!context.stack.isEmpty) {
            // initial value is on the stack, set it
            context.vars.variables += new Variable(name, context.stack.pop, typeName)
          } else {
            context.vars.variables += new Variable(name, initial, typeName)
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
    
    var direction: Direction = Entering

    def tick(): Unit = {
      direction = if (mainContext.vars.nodes.contains(current)) Exiting else Entering
      
      //println("BEGIN: " + current.getClass.getSimpleName + ":" + direction)   
      
      val paths: Seq[IASTNode] = step(current, mainContext, direction)   
      
      if (isBreaking) {
        // unroll the path stack until we meet the first parent which is a loop
        var reverse = pathStack.pop
        while (!reverse.isInstanceOf[IASTWhileStatement] && !reverse.isInstanceOf[IASTWhileStatement]) {
          reverse = pathStack.pop
        }
        isBreaking = false
      }
      
      if (direction == Exiting) {
        pathStack.pop
      } else {
        mainContext.vars.nodes += current
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
        //println(current.getClass.getSimpleName + ":" + direction)
        tick()
      }
    }
    
    current = tUnit
    
    mainContext.visitedStack.push(Visited(new ListBuffer[IASTNode](), new ListBuffer[Var](), new ListBuffer[Var]())) // load initial stack
    mainContext.vars = mainContext.visitedStack.head

    runProgram()
    isPreprocessing = false
    mainContext.stack.clear
    
    println("_----------------------------------------------_")
    
    mainContext.visitedStack.clear
    mainContext.visitedStack.push(Visited(new ListBuffer[IASTNode](), new ListBuffer[Var](), mainContext.vars.variables.clone)) // load initial stack
    mainContext.vars = mainContext.visitedStack.head
    pathStack.clear
    pathStack.push(mainContext.functionMap("main"))
    current = pathStack.head

    runProgram()
  }
}
