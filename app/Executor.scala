package app.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;
import java.nio.ByteBuffer
import java.nio.ByteOrder
import org.eclipse.cdt.internal.core.dom.parser.c._
import java.math.BigInteger

case class VarRef(name: String)
case class Literal(lit: String) {
  def cast: Any = {
    
    def isIntNumber(s: String): Boolean = (allCatch opt s.toInt).isDefined
    def isLongNumber(s: String): Boolean = (allCatch opt s.toLong).isDefined
    def isDoubleNumber(s: String): Boolean = (allCatch opt s.toDouble).isDefined
    
    if (lit.head == '\"' && lit.last == '\"') {
      lit
    } else if (lit.head == '\'' && lit.last == '\'' && lit.length == 3) {
      lit.toCharArray.apply(1)
    } else if (isIntNumber(lit)) {
      lit.toInt
    } else if (isLongNumber(lit)) {
      lit.toLong
    } else {
      lit.toDouble
    }
  }
}

class State {
  val stack = new Stack[Any]()
  val rawDataStack = new VarStack
  val executionContext = new Stack[FunctionExecutionContext]()
  val globals = new ListBuffer[Variable]()  
  var vars: FunctionExecutionContext = null
  val functionMap = scala.collection.mutable.Map[String, IASTNode]()
  val stdout = new ListBuffer[String]()
  
  // flags
  var isBreaking = false
  var isContinuing = false
  var isPreprocessing = true
  
  var parsingAssignmentDest = false
  
  def callFunction(call: IASTFunctionCallExpression) = {
    executionContext.push(vars)
    vars = new FunctionExecutionContext(globals)
    
    val name = call.getFunctionNameExpression match {
      case x: IASTIdExpression => x.getName.getRawSignature
      case _ => "Error"
    }
    
    Seq(functionMap(name))
  }
  
  def clearVisited(parent: IASTNode) {
    vars.visited -= parent
    parent.getChildren.foreach { node =>
      clearVisited(node)
    }
  }
  
  class VarStack {
    private val data = ByteBuffer.allocate(10240);
    data.order(ByteOrder.LITTLE_ENDIAN)
    
    var insertIndex = 0
    
    case class MemRange(start: Int, end: Int, theType: IBasicType)
  
    private val records = new ListBuffer[MemRange]()
    
    def getType(address: Address): IBasicType = {
      records.find{range => range.start <= address.address && range.end >= address.address}.get.theType
    }
    
    def getSize(address: Address): Int = {
      val range = records.find{range => range.start <= address.address && range.end >= address.address}.get
      range.end - range.start + 1
    }
    
    def allocateSpace(theType: IBasicType, typeName: String, numElements: Int): Address = {
      val result = insertIndex
      insertIndex += TypeHelper.sizeof(typeName) * numElements
      records += MemRange(result, insertIndex - 1, theType)
      Address(result)
    }
    
    def readVal(address: Int): Any = {
      val typeName = TypeResolver.resolve(getType(Address(address))).toString
      
      typeName match {
        case "int" | "unsigned int" => data.getInt(address)
        case "short int"  => data.getShort(address)
        case "unsigned short" | "unsigned short int" => data.getShort(address) & 0xFFFF
        case "double" => data.getDouble(address)
        case "char" => data.getChar(address)
        case "unsigned char" => data.getChar(address) & 0xFF
      }
    }
    
    // use Address type to prevent messing up argument order
    def setValue(newVal: Any, address: Address): Unit = newVal match {
      case newVal: Long => data.putInt(address.address, newVal.toInt) // BUG - dealing with unsigned int
      case newVal: Int => data.putInt(address.address, newVal)
      case newVal: Double => data.putDouble(address.address, newVal)
      case newVal: Char => data.putChar(address.address, newVal)
      case newVal: Boolean => data.putChar(address.address, if (newVal) 1 else 0)
    }
  }
}

case class Address(address: Int)

object TypeHelper {
  def sizeof(typeName: String) = typeName match {
    case "int" | "unsigned int" => 4
    case "short int" | "unsigned short" | "unsigned short int" => 2
    case "double" => 8
    case "float" => 4
    case "bool" => 4
    case "char" | "unsigned char" => 1
  }
}

object TypeResolver {
   def resolve(theType: IType): IBasicType = theType match {
    case basicType: IBasicType => basicType
    case typedef: ITypedef => resolve(typedef.getType)
    case ptrType: IPointerType => resolve(ptrType.getType)
    case arrayType: IArrayType => resolve(arrayType.getType)
    case qualType: IQualifierType => resolve(qualType.getType)
  }
}

protected class Variable(stack: State#VarStack, val name: String, val theType: IType, val numElements: Int) {
  
  val typeName = TypeResolver.resolve(theType).toString
  
  val isPointer = theType.isInstanceOf[IPointerType] || theType.isInstanceOf[IArrayType]
  
  val address: Address = if (isPointer) {
    val intType = new CBasicType(IBasicType.Kind.eInt , 0) 
    stack.allocateSpace(intType, "int", numElements)
  } else {
    stack.allocateSpace(TypeResolver.resolve(theType), typeName, numElements)
  }
  
  val size = if (isPointer) {
    4
  } else {
    TypeHelper.sizeof(typeName)
  }
  
  def value: Any = {
    stack.readVal(address.address)
  }
  
  def getArray: Array[Any] = {
    var i = 0
    (0 until numElements).map{ element => 
      val result = stack.readVal(address.address + i)
      i += size
      result
    }.toArray
  }
  
  def dereference: Any = stack.readVal(value.asInstanceOf[Int])
  
  
  
  def setValue(value: Any): Unit = {
    
    val theVal = value match {
      case lit @ Literal(literal) => 
        if (theType == null) {
          lit.cast
        } else {
          TypeResolver.resolve(theType).toString match {
            case "double" => literal.toDouble
            case "int" | "unsigned int" => 
              if (literal.startsWith("0x")) { 
                val bigInt = new BigInteger(literal.drop(2), 16);
                bigInt.intValue
              } else {
                literal.toInt
              }
            case "float" => literal.toFloat
            case _ => lit.cast
          }
        }
      case x => x
    }
    
    theVal match {
      case newVal: Int => stack.setValue(newVal, address)
      case newVal: Long => stack.setValue(newVal, address)
      case newVal: Double => stack.setValue(newVal, address)
      case newVal: Char => stack.setValue(newVal, address)
      case newVal: Boolean => stack.setValue(if (newVal) 1 else 0, address)
      case address @ Address(addy) => setValue(addy)
      case array: Array[_] =>
        var i = 0
        array.foreach{element =>  element match {
          case lit @ Literal(_) =>
            stack.setValue(lit.cast, Address(address.address + i))
            i += size
          case x =>
            stack.setValue(x, Address(address.address + i))
            i += size
        }}
    }
  }
  
  def setArrayValue(value: Any, index: Int) = {
    stack.setValue(value, Address(address.address + index * size))
  }
  
  def sizeof: Int = {  
    if (isPointer) {
      stack.getSize(Address(value.asInstanceOf[Int]))
    } else {
      stack.getSize(address)
    }
  }
}



class FunctionExecutionContext(globals: Seq[Variable]) {
  
  val visited = new ListBuffer[IASTNode]()
  val variables: ListBuffer[Variable] = ListBuffer[Variable]() ++ (if (globals == null) Seq() else globals)

  def addVariable(stack: State#VarStack, theName: String, theValue: Any, theType: IType): Variable = {
    
    val typeName = theType.toString

    val newVar = theValue match {
      case array: Array[_] =>
        val theArray = new Variable(stack, theName + "_array", theType.asInstanceOf[IArrayType].getType, array.length)
        theArray.setValue(theValue)
        val theArrayPtr = new Variable(stack, theName, theType, 1)
        theArrayPtr.setValue(theArray.address)
        theArrayPtr
      case VarRef(variableName) =>
        val newVar = new Variable(stack, theName, theType, 1)
        newVar.setValue(resolveId(variableName).value)
        newVar
      case _ =>
        val newVar = new Variable(stack, theName, theType, 1)
        newVar.setValue(theValue)
        newVar
    }
    
    variables += newVar
    newVar
  }

  def resolveId(id: String): Variable = {
    variables.find(_.name == id).get
  }
}


object Executor {
  
  // 'node' must be a IASTCaseStatement or a IASTDefaultStatement
  def processSwitch(node: IASTNode): Seq[IASTNode] = {
    val codeToRun = new ListBuffer[IASTNode]()

      val siblings = node.getParent.getChildren
      
      var isSelfFound = false
      siblings.foreach{ sib =>
        if (sib == node) {
          isSelfFound = true
        } else if (isSelfFound && !sib.isInstanceOf[IASTCaseStatement]) {
          codeToRun += sib
        }
      }
      
      codeToRun
  }
  
  def parseStatement(statement: IASTStatement, state: State, direction: Direction): Seq[IASTNode] = statement match {
    case breakStatement: IASTBreakStatement =>
      state.isBreaking = true
      Seq()
    case continueStatement: IASTContinueStatement =>
      state.isContinuing = true
      Seq()
    case switch: IASTSwitchStatement =>
      val cases = switch.getBody.getChildren.collect{case x: IASTCaseStatement => x; case y: IASTDefaultStatement => y} 
      if (direction == Entering) {       
        Seq(switch.getControllerExpression) ++ cases // only process case and default statements
      } else {
        Seq()
      }
    case default: IASTDefaultStatement =>
      if (direction == Exiting) {       
        processSwitch(default)
      } else {
        Seq()
      }
    case caseStatement: IASTCaseStatement =>
      if (direction == Entering) {
        Seq(caseStatement.getExpression)
      } else {
       
        val caseExpr = state.stack.pop.asInstanceOf[Literal].cast
        val switchExpr = state.stack.pop
        
        state.stack.push(switchExpr)
        
        val resolved = switchExpr match {
          case VarRef(x) => state.vars.resolveId(x).value
          case int: Int => int
        }

        if (caseExpr.asInstanceOf[Int] == resolved.asInstanceOf[Int]) {
          processSwitch(caseStatement)
        } else {
          Seq()
        }
      }
    case doWhileLoop: IASTDoStatement =>
      if (direction == Entering) {
        Seq(doWhileLoop.getBody, doWhileLoop.getCondition)
      } else {
        val shouldLoop = state.stack.pop match {
          case x: Int => x == 1
          case x: Boolean => x
        }
      
        if (shouldLoop) {
          state.clearVisited(doWhileLoop.getBody)
          state.clearVisited(doWhileLoop.getCondition)
          
          Seq(doWhileLoop.getBody, doWhileLoop.getCondition, doWhileLoop)
        } else {
          Seq()
        }
      }
    case whileLoop: IASTWhileStatement =>
      if (direction == Entering) {
        Seq(whileLoop.getCondition)
      } else {
        
        val cast = state.stack.pop match {
          case lit @ Literal(_) => lit.cast
          case x => x
        }
        
        val shouldLoop = cast match {
          case x: Int => x == 1
          case x: Boolean => x
        }
      
        if (shouldLoop) {
          state.clearVisited(whileLoop.getBody)
          state.clearVisited(whileLoop.getCondition)
          
          Seq(whileLoop.getBody, whileLoop.getCondition, whileLoop)
        } else {
          Seq()
        }
      }
    case ifStatement: IASTIfStatement =>
      if (direction == Entering) {
        Seq(ifStatement.getConditionExpression)
      } else {
        val result = state.stack.pop
        
        val value = result match {
          case VarRef(name) =>
            state.vars.resolveId(name).value
          case lit @ Literal(_) =>
            lit.cast
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
        val shouldKeepLooping = state.stack.pop.asInstanceOf[Boolean]
      
        if (shouldKeepLooping) {
          state.clearVisited(forLoop.getBody)
          state.clearVisited(forLoop.getIterationExpression)
          state.clearVisited(forLoop.getConditionExpression)
          
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
        val returnVal = state.stack.pop
        state.stack.push(returnVal match {
          case lit @ Literal(_) => lit.cast
          case VarRef(id) => state.vars.resolveId(id).value
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
  
 
  
  def parseDeclarator(decl: IASTDeclarator, direction: Direction, state: State): Seq[IASTNode] = {
    val nameBinding = decl.getName.resolveBinding()

    if (direction == Exiting && nameBinding.isInstanceOf[IVariable]) {
      
      val currentType = nameBinding.asInstanceOf[IVariable].getType
      
      state.stack.push(decl.getName.getRawSignature)
      
      val resolved = TypeResolver.resolve(currentType)

      val initial = if (resolved.isLong) {
        0.toInt
      } else if (resolved.isLongLong) {
        0.0.toDouble
      } else if (resolved.isShort) {
        0.toShort
      } else {
        0.toChar
      }
      
      val name = state.stack.pop.asInstanceOf[String]
      
      val initVal = if (decl.isInstanceOf[IASTArrayDeclarator]) {
        
        state.stack.pop.asInstanceOf[Literal].cast match {
          case size: Int =>
            val initialArray = Array.fill[Any](size)(initial)
            
            if (!state.stack.isEmpty) { 
              var i = 0
              for (i <- (size - 1) to 0 by -1) {
                val newInit = state.stack.pop
                initialArray(i) = newInit
              }
            }
            initialArray
          case initString: String =>
            Utils.stripQuotes(initString).toCharArray() :+ 0.toChar // terminating null char
        }
      } else {   
        if (!decl.getPointerOperators.isEmpty) {
          if (!state.stack.isEmpty) {
            state.stack.pop
          } else {
            0
          }
        } else {
          if (!state.stack.isEmpty) {
            state.stack.pop
          } else {
            initial
          }
        }
      }
      
      state.vars.addVariable(state.rawDataStack, name, initVal, currentType)
      
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
  
  def step(current: IASTNode, state: State, direction: Direction): Seq[IASTNode] = {

    current match {
      case statement: IASTStatement =>
        Executor.parseStatement(statement, state, direction)
      case expression: IASTExpression =>
        Expressions.parse(expression, direction, state, state.rawDataStack)
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
          val arg = state.stack.pop 
          val paramInfo = param.getDeclarator.getName.resolveBinding().asInstanceOf[CParameter]
          state.vars.addVariable(state.rawDataStack, param.getDeclarator.getName.getRawSignature, arg, paramInfo.getType)
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
        Executor.parseDeclarator(decl, direction, state)
      case fcnDef: IASTFunctionDefinition =>
        if (state.isPreprocessing) {
          state.functionMap += (fcnDef.getDeclarator.getName.getRawSignature -> fcnDef)
          Seq()
        } else if (direction == Exiting) {
          state.vars = state.executionContext.pop
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
           state.stack.push(typeId.getDeclSpecifier.getRawSignature)
        }
        Seq()
      case spec: IASTSimpleDeclSpecifier =>
        if (direction == Entering) {
          Seq()
        } else {
          state.stack.push(spec.getRawSignature)
          Seq()
        }
    }
  }
}

class Executor(code: String) {
   
  val tUnit = Utils.getTranslationUnit(code)
  val engineState = new State
  val pathStack = new Stack[IASTNode]()
  var current: IASTNode = null
  var direction: Direction = Entering 
  
  current = tUnit
    
  engineState.executionContext.push(new FunctionExecutionContext(null)) // load initial stack
  engineState.vars = engineState.executionContext.head

  execute()
  engineState.isPreprocessing = false
  engineState.stack.clear
  
  println("_----------------------------------------------_")
  
  engineState.globals ++= engineState.vars.variables
  
  engineState.executionContext.clear
  engineState.executionContext.push(new FunctionExecutionContext(engineState.globals)) // load initial stack
  engineState.vars = engineState.executionContext.head
  pathStack.clear
  pathStack.push(engineState.functionMap("main"))
  current = pathStack.head
  
  def tick(): Unit = {
    direction = if (engineState.vars.visited.contains(current)) Exiting else Entering

    //println(current.getClass.getSimpleName + ":" + direction)
    
    var paths: Seq[IASTNode] = Executor.step(current, engineState, direction)   
    
    if (engineState.isBreaking) {
      // unroll the path stack until we meet the first parent which is a loop
      var reverse = pathStack.pop
      while (!reverse.isInstanceOf[IASTWhileStatement] && !reverse.isInstanceOf[IASTForStatement] && !reverse.isInstanceOf[IASTSwitchStatement]) {
        reverse = pathStack.pop
      }

      engineState.isBreaking = false
    }
    
    if (engineState.isContinuing) {
      // unroll the path stack until we meet the first parent which is a loop
      
      var last: IASTNode = null
      last = pathStack.pop
      while (!last.isInstanceOf[IASTForStatement]) {
        last = pathStack.pop
      }
      
      val forLoop = last.asInstanceOf[IASTForStatement]
      
      pathStack.push(forLoop)
      pathStack.push(forLoop.getConditionExpression)
      pathStack.push(forLoop.getIterationExpression) 

      engineState.isContinuing = false
    }
    
    if (direction == Exiting) {
      pathStack.pop
    } else {
      engineState.vars.visited += current
    }
    
    paths.reverse.foreach{path => pathStack.push(path)}
    
    if (!pathStack.isEmpty) {
      current = pathStack.head
    } else {
      current = null
    }
  }

  def execute() = {
    while (current != null) {
      tick()
    }
  }
}
