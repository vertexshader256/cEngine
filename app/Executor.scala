package app.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;
import java.nio.ByteBuffer
import java.nio.ByteOrder

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

class VarStack {
  private val data = ByteBuffer.allocate(10240);
  data.order(ByteOrder.LITTLE_ENDIAN)
  
  case class MemRange(start: Int, end: Int, typeName: String)
  
  private val records = new ListBuffer[MemRange]()
  
  def getType(address: Address): String = {
    records.find{range => range.start <= address.address && range.end > address.address}.get.typeName
  }
  
  var insertIndex = 0
  
  case class Record(address: Address, typeName: String)
  
  def allocateSpace(typeName: String, numElements: Int): Address = {
    val result = insertIndex
    insertIndex += TypeHelper.sizeof(typeName) * numElements
    records += MemRange(result, insertIndex, typeName)
    Address(result)
  }
  
  def readVal(address: Int): Any = {
    val typeName = getType(Address(address))
    
    typeName match {
      case "int" => data.getInt(address)
      case "double" => data.getDouble(address)
      case "char" => data.getChar(address).toInt.toChar
    }
  }
  
  // use Address type to prevent messing up argument order
  def setValue(newVal: Any, address: Address): Unit = newVal match {
    case newVal: Int => data.putInt(address.address, newVal)
    case newVal: Double => data.putDouble(address.address, newVal)
    case newVal: Char => data.putChar(address.address, newVal)
    case newVal: Boolean => data.putChar(address.address, if (newVal) 1 else 0)
  }
  
  def unapply(vari: Variable): Option[Any] = if (vari eq null) None else Some(vari.value)
}

case class Address(address: Int)

protected class Variable(stack: VarStack, val name: String, val typeName: String, val numElements: Int, val isPointer: Boolean) {
  
  val address: Address = stack.allocateSpace(typeName, numElements)
  
  def value: Any = {
    stack.readVal(address.address)
  }
  
  def getArray: Array[Any] = {
    var i = 0
    (0 until numElements).map{ element => 
      val result = stack.readVal(address.address + i)
      i += TypeHelper.sizeof(typeName)
      result
    }.toArray
  }
  
  def dereference: Any = stack.readVal(value.asInstanceOf[Int])
  
  def setValue(newVal: Any): Unit = newVal match {
    case newVal: Int => stack.setValue(newVal, address)
    case newVal: Double => stack.setValue(newVal, address)
    case newVal: Char => stack.setValue(newVal, address)
    case newVal: Boolean => stack.setValue(if (newVal) 1 else 0, address)
    case address @ Address(addy) => setValue(addy)
    case array: Array[_] =>
      var i = 0
      array.foreach{element =>  stack.setValue(element, Address(address.address + i))
        i += TypeHelper.sizeof(typeName)
      }
  }
  
  def setArrayValue(value: Any, index: Int) = {
    stack.setValue(value, Address(address.address + index * TypeHelper.sizeof(typeName)))
  }
  
  def sizeof: Int = {
    if (isPointer) {
      if (numElements > 1) {
        TypeHelper.sizeof(typeName) * numElements
      } else {
        4
      }
    } else {
      TypeHelper.sizeof(typeName) * numElements
    }
  }
}

case class VarRef(name: String)

class Visited(parent: Visited) {
  
  val nodes = new ListBuffer[IASTNode]()
  private val functionArgs = new ListBuffer[Variable]()
  private val variables: ListBuffer[Variable] = ListBuffer[Variable]() ++ Option(parent).map(x => x.variables).getOrElse(Seq())
  
  def addArg(stack: VarStack, theName: String, theValue: Any, theTypeName: String, isPointer: Boolean) = {
    val newArg = new Variable(stack, theName, theTypeName, 1, isPointer)
    newArg.setValue(theValue)
    functionArgs += newArg
  }
  
  def getArg(name: String): Variable = {
    functionArgs.find(_.name == name).get
  }
  
  def addVariable(stack: VarStack, theName: String, theValue: Any, theTypeName: String, isPointer: Boolean) = {
    val newVar = theValue match {
      case array: Array[_] => new Variable(stack, theName, theTypeName, array.length, true)
      case _ => new Variable(stack, theName, theTypeName, 1, isPointer)
    }
    newVar.setValue(theValue)
    variables += newVar
  }

  def resolveId(id: String): Variable = {
    if (variables.exists(_.name == id)) {
      variables.find(_.name == id).get
    } else {
      getArg(id)
    }
  }
  
  def resolveAddress(address: Address): Variable = {
    variables.find{vari =>
      var startAddy = vari.address.address
      var endAddy = vari.address.address + TypeHelper.sizeof(vari.typeName) * vari.numElements
      startAddy <= address.address && endAddy > address.address
    }.getOrElse(functionArgs.find(_.address == address).getOrElse(null))
  }
}

class Executor(code: String) {
  
  val stack = new VarStack
  
  
  var isPreprocessing = true
  val tUnit = Utils.getTranslationUnit(code)

  val mainContext = new IASTContext(tUnit)
  
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
        Expressions.parse(expression, direction, context, stack)
      case array: IASTArrayModifier =>
        if (direction == Exiting) {
          Seq()
        } else {
          if (array.getConstantExpression != null) {
            Seq(array.getConstantExpression)
          } else {
            // e.g char str[] = "test"
            Seq()
          }
        }
      case param: IASTParameterDeclaration =>
        if (direction == Exiting) {
          val arg = context.stack.pop
          if (!param.getDeclarator.getPointerOperators.isEmpty) {
             context.vars.addArg(stack, param.getDeclarator.getName.getRawSignature, arg.asInstanceOf[Address], param.getDeclSpecifier.getRawSignature, true)
          } else {
             context.vars.addArg(stack, param.getDeclarator.getName.getRawSignature, arg, param.getDeclSpecifier.getRawSignature, false)
          }
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
      
      if (decl.isInstanceOf[IASTArrayDeclarator]) {
        val name = context.stack.pop.asInstanceOf[String]
        
        context.stack.pop match {
          case size: Int =>
            val initialArray = Array.fill[Any](size)(initial)
            
            if (!context.stack.isEmpty) { 
              var i = 0
              for (i <- (size - 1) to 0 by -1) {
                val newInit = context.stack.pop
                initialArray(i) = newInit
              }
            }
            context.vars.addVariable(stack, name, initialArray, theTypeName, false)
          case initString: String =>
            val initialArray = Utils.stripQuotes(initString).toCharArray() :+ 0.toChar // terminating null char
            context.vars.addVariable(stack, name, initialArray, theTypeName, false)
        }
      } else {   
        
        val name = context.stack.pop.asInstanceOf[String]
        
        if (!decl.getPointerOperators.isEmpty) {
          if (!context.stack.isEmpty) {
            val initVal = context.stack.pop
            context.vars.addVariable(stack, name, initVal, theTypeName, true)
          } else {
            context.vars.addVariable(stack, name, 0, theTypeName, true)
          }
        } else {
          if (!context.stack.isEmpty) {
            // initial value is on the stack, set it
            context.vars.addVariable(stack, name, context.stack.pop, theTypeName, false)
          } else {
            context.vars.addVariable(stack, name, initial, theTypeName, false)
          }
        }
      }
      
      Seq()
    } else {
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
       // println(current.getClass.getSimpleName + ":" + direction)
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
