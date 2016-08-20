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
    vars = new Visited(vars)
    
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

object Variable {
  def unapply(vari: Variable): Option[Any] = if (vari eq null) None else Some(vari.value)
}

abstract class Variable {
  
  var realValue: Any = null
  
  val name: String
  def value: Any = realValue
  val typeName: String
  
  def setValue(newVal: Any) = {
    realValue = newVal
  }
  
  def sizeof: Int = value match {
    case Variable(theValue) => 4 // its a pointer
    case array: Array[Variable] => array.length * array.head.sizeof
    case _ => 
      TypeHelper.sizeof(typeName)
  }
}

case class VarRef(name: String)

class Visited(parent: Visited) {
  
  val nodes = new ListBuffer[IASTNode]()
  private val functionArgs = new ListBuffer[Variable]()
  private val variables: ListBuffer[Variable] = ListBuffer[Variable]() ++ Option(parent).map(x => x.variables).getOrElse(Seq())
  
  def addArg(theName: String, theValue: Any, theTypeName: String) = {
    functionArgs += new Variable { val name = theName; realValue = theValue; val typeName = theTypeName }
  }
  
  def getArg(name: String): Variable = {
    functionArgs.find(_.name == name).get
  }
  
  def addVariable(theName: String, theValue: Any, theTypeName: String) = {
    variables += new Variable { val name = theName; realValue = theValue; val typeName = theTypeName }
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
          context.vars.addArg(param.getDeclarator.getName.getRawSignature, arg, param.getDeclSpecifier.getRawSignature)
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
      
      val theTypeName = context.currentType.getRawSignature
      
      val initial = theTypeName match {
          case "int" => 0.toInt
          case "double" => 0.0.toDouble
          case "char" => 0.toChar
          case _ => throw new Exception("No match for " + theTypeName)
      }
      
      if (isArrayDeclaration) {
        
        val name = context.stack.pop.asInstanceOf[String]
        
        val size = context.stack.pop.asInstanceOf[Int]
       
        val initialArray = Array.fill(size)(new Variable { val name = ""; realValue = initial; val typeName = theTypeName })
        
        if (!context.stack.isEmpty) { 
          var i = 0
          
          for (i <- (size - 1) to 0 by -1) {
            initialArray(i).setValue(context.stack.pop)
          }
        }
        
        context.vars.addVariable(name, initialArray, theTypeName)
      } else {   
        
        val name = context.stack.pop.asInstanceOf[String]
        
        if (!decl.getPointerOperators.isEmpty) {
          if (!context.stack.isEmpty) {
            // initial value is on the stack, set it
            context.vars.addVariable(name, context.stack.pop.asInstanceOf[Variable], theTypeName)
          } else {
            context.vars.addVariable(name, null, theTypeName)
          }
        } else {
          if (!context.stack.isEmpty) {
            // initial value is on the stack, set it
            context.vars.addVariable(name, context.stack.pop, theTypeName)
          } else {
            context.vars.addVariable(name, initial, theTypeName)
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
    
    mainContext.visitedStack.push(new Visited(null)) // load initial stack
    mainContext.vars = mainContext.visitedStack.head

    runProgram()
    isPreprocessing = false
    mainContext.stack.clear
    
    println("_----------------------------------------------_")
    
    mainContext.visitedStack.clear
    mainContext.visitedStack.push(new Visited(mainContext.vars)) // load initial stack
    mainContext.vars = mainContext.visitedStack.head
    pathStack.clear
    pathStack.push(mainContext.functionMap("main"))
    current = pathStack.head

    runProgram()
  }
}
