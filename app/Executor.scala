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
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._

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
    } else if (lit.contains('F') || lit.contains('f')) {
      val num = lit.toCharArray.filter(x => x != 'f' && x != 'F').mkString
      num.toFloat
    } else {
      lit.toDouble
    }
  }
}

class State {
  val stack = new Stack[Any]()
  val executionContext = new Stack[FunctionExecutionContext]()
  val globals = new ListBuffer[RuntimeVariable]()  
  var vars: FunctionExecutionContext = null
  val functionMap = scala.collection.mutable.Map[String, IASTNode]()
  val stdout = new ListBuffer[String]()
  
  // flags
  var isBreaking = false
  var isContinuing = false
  var isPreprocessing = true

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
  
  private val data = ByteBuffer.allocate(10240);
  data.order(ByteOrder.LITTLE_ENDIAN)
  
  var insertIndex = 0
  
  private case class MemRange(start: Int, end: Int)

  private val records = new ListBuffer[MemRange]()
  
  def getSize(address: Address): Int = {
    val range = records.find{range => range.start <= address.value && range.end >= address.value}.get
    range.end - range.start + 1
  }
  
  def allocateSpace(theType: IBasicType, numElements: Int): Address = {
    val result = insertIndex
    insertIndex += TypeHelper.sizeof(theType) * numElements
    records += MemRange(result, insertIndex - 1)
    Address(result)
  }
  
  def readVal(address: Int, theType: IBasicType): Any = {

    import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._

    // if it is neither signed or unsigned, assume its signed
    val isSigned = theType.isSigned || (!theType.isSigned && !theType.isUnsigned)
    
    if (theType.isShort && isSigned) {
      data.getShort(address)
    } else if (theType.isShort && !isSigned) {
      data.getShort(address) & 0xFFFF
    } else if (theType.getKind == eInt) {
      data.getInt(address)
    } else if (theType.getKind == eDouble) {
      data.getDouble(address)
    } else if (theType.getKind == eFloat) {
      data.getFloat(address)
    } else if (isSigned) {
      data.getChar(address)
    } else if (!isSigned) {
      data.getChar(address) & 0xFF
    }
  }
  
  // use Address type to prevent messing up argument order
  def setValue(newVal: Any, address: Address): Unit = newVal match {
    case newVal: Char => data.put(address.value, newVal.toByte) // MUST convert to byte because writing char is 2 bytes!!!
    case newVal: Long => data.putInt(address.value, newVal.toInt) // BUG - dealing with unsigned int
    case newVal: Int => data.putInt(address.value, newVal)
    case newVal: Float => data.putFloat(address.value, newVal)
    case newVal: Double => data.putDouble(address.value, newVal)
    
    case newVal: Boolean => data.putChar(address.value, if (newVal) 1 else 0)
  }
}

case class Address(value: Int) {
  def +(x: Int) = {
    Address(value + x)
  }
}
case class AddressInfo(address: Address, theType: IType)

object TypeHelper {
  
  def resolve(theType: IType): IBasicType = theType match {
    case basicType: IBasicType => basicType
    case typedef: ITypedef => resolve(typedef.getType)
    case ptrType: IPointerType => resolve(ptrType.getType)
    case arrayType: IArrayType => resolve(arrayType.getType)
    case qualType: IQualifierType => resolve(qualType.getType)
  }
  
  def sizeof(theType: IType): Int = theType match {
    case ptr: IPointerType =>
      4
    case struct: CStructure =>
      struct.getFields.map{ field =>
        sizeof(field.getType)
      }.sum
    case array: IArrayType =>
      sizeof(array.getType)
    case typedef: CTypedef =>
      sizeof(typedef.getType)  
    case qual: IQualifierType =>
      sizeof(qual.getType)
    case basic: IBasicType =>
      basic.getKind match {
        case `eInt` => 4
        case `eFloat` => 4
        case `eChar16` => 2
        case `eDouble` => 8
        case `eChar` => 1
        case `eChar32` => 4
      }
  }
  
  
}

trait RuntimeVariable {
  def address: Address
  def name: String
  def value: Any
  def isPointer: Boolean
  def theType: IType
  def state: State
  def setValue(value: Any): Unit
  
  val size = TypeHelper.sizeof(theType)
  
  def sizeof: Int
  
  def allocateSpace(aType: IType, numElements: Int): Address = {
    if (aType.isInstanceOf[IArrayType] || aType.isInstanceOf[IPointerType]) {
      val intType = new CBasicType(IBasicType.Kind.eInt , 0) 
      state.allocateSpace(intType, 1)
    } else if (aType.isInstanceOf[CStructure]) {
      val struct = aType.asInstanceOf[CStructure]
      var result: Address = null
      struct.getFields.foreach{ field =>
        if (result == null) {
          result = allocateSpace(field.getType, numElements)
        } else {
          allocateSpace(field.getType, numElements)
        }
      }
      result
    } else if (aType.isInstanceOf[CTypedef]) {
      allocateSpace(aType.asInstanceOf[CTypedef].getType, numElements)
    } else {
      state.allocateSpace(TypeHelper.resolve(aType), numElements)
    }
  }
}

protected class ArrayVariable(val state: State, val name: String, val theType: IType, val numElements: Int) extends RuntimeVariable {
  
  // In C arrays are pointers
  val isPointer = true
  
  // where we store the actual data
  val theArrayAddress = allocateSpace(theType.asInstanceOf[IArrayType].getType, numElements)

  // where we store the reference
  val address: Address = allocateSpace(theType, 1)
  
  state.setValue(theArrayAddress.value, address)

  def sizeof: Int = {  
    state.getSize(theArrayAddress)
  }

  def setValue(value: Any): Unit = value match {
    case array: Array[_] =>
      var i = 0
      array.foreach{element =>  element match {   
        case lit @ Literal(_) =>
          state.setValue(lit.cast, Address(theArrayAddress.value + i))        
        case x =>
          state.setValue(x, Address(theArrayAddress.value + i))
        }
        i += TypeHelper.sizeof(theType.asInstanceOf[IArrayType].getType)
      }
  }
  
  // for value lets return the 4-byte address of the data
  def value: Any = {
    state.readVal(address.value, new CBasicType(IBasicType.Kind.eInt , 0))
  }
}

protected class Variable(val state: State, val name: String, val theType: IType) extends RuntimeVariable {
  
  val isPointer = theType.isInstanceOf[IPointerType]
  
  val address: Address = allocateSpace(theType, 1)
  
  val resolved = {
    if (isPointer) {
      new CBasicType(IBasicType.Kind.eInt , 0)
    } else {
      TypeHelper.resolve(theType)
    }
  }
  
  def sizeof: Int = {  
    if (isPointer) {
      state.getSize(Address(value.asInstanceOf[Int]))
    } else {
      state.getSize(address)
    }
  }
  
  def value: Any = {
      state.readVal(address.value, resolved)
  }

  def setValue(value: Any): Unit = {
    
    val theVal = value match {
      case lit @ Literal(literal) => 
        if (theType == null) {
          lit.cast
        } else {
          TypeHelper.resolve(theType).getKind match {
            case `eDouble` => literal.toDouble
            case `eInt` => 
              if (literal.startsWith("0x")) { 
                val bigInt = new BigInteger(literal.drop(2), 16);
                bigInt.intValue
              } else {
                literal.toInt
              }
            case `eFloat` => literal.toFloat
            case _ => lit.cast
          }
        }
      case x => x
    }
    
    theVal match {
      case x: Int => state.setValue(x, address)
      case x: Long => state.setValue(x, address)
      case x: Float => state.setValue(x, address)
      case x: Double => state.setValue(x, address)
      case x: Char => state.setValue(x, address)
      case x: Boolean => state.setValue(if (x) 1 else 0, address)
      case Address(addy) => setValue(addy)
      case AddressInfo(addy, _) => setValue(addy)
      case array: Array[_] =>
        var i = 0
        array.foreach{element =>  element match {
          case lit @ Literal(_) =>
            state.setValue(lit.cast, Address(address.value + i))
          case x =>
            state.setValue(x, Address(address.value + i))
          }
          i += size
        }
    }
  }
  
  
}

class FunctionExecutionContext(globals: Seq[RuntimeVariable]) { 
  val visited = new ListBuffer[IASTNode]()
  val variables: ListBuffer[RuntimeVariable] = ListBuffer[RuntimeVariable]() ++ (if (globals == null) Seq() else globals)

  def resolveId(id: String): RuntimeVariable = {
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

      val name = state.stack.pop.asInstanceOf[String]
      
      if (decl.isInstanceOf[IASTArrayDeclarator]) {
        
        val initVal = state.stack.pop.asInstanceOf[Literal].cast match {
          case size: Int =>
            val initialArray = Array.fill[Any](size)(0)
            
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
        
        val theArrayPtr = new ArrayVariable(state, name, currentType, initVal.length)
        theArrayPtr.setValue(initVal)
        
        state.vars.variables += theArrayPtr
        
        theArrayPtr
        
      } else {   
        val initVal = if (!decl.getPointerOperators.isEmpty) {
          if (!state.stack.isEmpty) {
            state.stack.pop
          } else {
            0
          }
        } else {
          if (!state.stack.isEmpty) {
            state.stack.pop
          } else {
            0
          }
        }
         
        val resolvedType = currentType match {
          case typedef: CTypedef => typedef.getType
          case x => x
        }
        
        def createVariables(theType: IType, name: String): Unit = theType match {
          case struct: CStructure =>
            struct.getFields.foreach{ field =>
              createVariables(field.getType, name + "_" + field.getName)
            }
          case typedef: CTypedef =>
            createVariables(typedef.getType, name)
          case _ =>
            val resolved = initVal match {
              case VarRef(name) =>
                state.vars.resolveId(name).value
              case x =>
                x
            }
            
            val newVar = new Variable(state, name, theType)
            newVar.setValue(resolved)
            state.vars.variables += newVar
        }
        
        createVariables(resolvedType, name)
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
  
  def step(current: IASTNode, state: State, direction: Direction): Seq[IASTNode] = {

    current match {
      case statement: IASTStatement =>
        Executor.parseStatement(statement, state, direction)
      case expression: IASTExpression =>
        Expressions.parse(expression, direction, state, state)
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
          
          val newVar = new Variable(state, param.getDeclarator.getName.getRawSignature, paramInfo.getType)
          newVar.setValue(arg)
          
          state.vars.variables += newVar
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
          val theType = typeId.getRawSignature

          val result = theType match {
            case "int" => 4
            case "float" => 4
            case "double" => 8
            case "short" => 2
            case x => 
              1
          }

          state.stack.push(result)
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
