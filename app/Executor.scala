package app.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ ListBuffer, Stack }
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;
import java.nio.ByteBuffer
import java.nio.ByteOrder
import org.eclipse.cdt.internal.core.dom.parser.c._
import java.math.BigInteger
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._
import scala.collection.mutable.Map

case class VarRef(name: String)
case class StringLiteral(str: String) extends AnyVal

case class Literal(lit: String) {
  def cast: AnyVal = {

    def isIntNumber(s: String): Boolean = (allCatch opt s.toInt).isDefined
    def isLongNumber(s: String): Boolean = (allCatch opt s.toLong).isDefined
    def isDoubleNumber(s: String): Boolean = (allCatch opt s.toDouble).isDefined

    if (lit.startsWith("0x")) {
      val bigInt = new BigInteger(lit.drop(2), 16);
      bigInt.intValue
    } else if (lit.head == '\"' && lit.last == '\"') {
      StringLiteral(lit)
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
  
  def typeCast(theType: IBasicType): AnyVal = {
    theType.getKind match {
      case `eDouble` => lit.toDouble
      case `eFloat` => lit.toFloat
      case _        => cast
    }
  }
}

class State {
  val stack = new Stack[Any]()
  val executionContext = new Stack[FunctionExecutionContext]()
  val globals = Map[String, RuntimeVariable]()
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
      case _                   => "Error"
    }

    Seq(functionMap(name))
  }

  def clearVisited(parent: IASTNode) {
    vars.visited -= parent
    parent.getChildren.foreach { node =>
      clearVisited(node)
    }
  }

  private val data = ByteBuffer.allocate(100000);
  data.order(ByteOrder.LITTLE_ENDIAN)

  var insertIndex = 0

  def allocateSpace(numBytes: Int): Address = {
    val result = insertIndex
    insertIndex += numBytes
    Address(result)
  }

  def readVal(address: Int, theType: IBasicType): AnyVal = {

    import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._

    // if it is neither signed or unsigned, assume its signed
    val isSigned = theType.isSigned || (!theType.isSigned && !theType.isUnsigned)

    if (theType.isShort && isSigned) {
      data.getShort(address)
    } else if (theType.isShort && !isSigned) {
      data.getShort(address) & 0xFFFF
    } else if (theType.getKind == eInt && theType.isLong) {
      data.getLong(address)
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
  def setValue(newVal: AnyVal, info: AddressInfo): Unit = {
    newVal match {
      case Address(addy)        => setValue(addy, info)
      case newVal: Char    => data.put(info.address.value, newVal.toByte) // MUST convert to byte because writing char is 2 bytes!!!
      case newVal: Long    => data.putLong(info.address.value, newVal)
      case newVal: Int     => data.putInt(info.address.value, newVal)
      case newVal: Float   => data.putFloat(info.address.value, newVal)
      case newVal: Double  => data.putDouble(info.address.value, newVal)
      case newVal: Boolean => data.putChar(info.address.value, if (newVal) 1 else 0)
    }
  }
}

case class Address(value: Int) extends AnyVal {
  def +(x: Int) = {
    Address(value + x)
  }
}
case class AddressInfo(address: Address, theType: IType)

object TypeHelper {

  def resolve(theType: IType): IBasicType = theType match {
    case basicType: IBasicType    => basicType
    case typedef: ITypedef        => resolve(typedef.getType)
    case ptrType: IPointerType    => resolve(ptrType.getType)
    case arrayType: IArrayType    => resolve(arrayType.getType)
    case qualType: IQualifierType => resolve(qualType.getType)
  }

  def sizeof(theType: IType): Int = theType match {
    case ptr: IPointerType =>
      4
    case struct: CStructure =>
      struct.getFields.map { field =>
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
        case `eInt` if basic.isLong => 8
        case `eInt`                 => 4
        case `eFloat`               => 4
        case `eChar16`              => 2
        case `eDouble`              => 8
        case `eChar`                => 1
        case `eChar32`              => 4
      }
  }
  
  def isPointer(theType: IType) = theType.isInstanceOf[IArrayType] || theType.isInstanceOf[IPointerType]
}

trait RuntimeVariable {
  val state: State
  val theType: IType
  def address: Address
  
  val size = TypeHelper.sizeof(theType)

  def sizeof: Int
  def info = AddressInfo(address, theType)
  
  def value: AnyVal = {
    if (TypeHelper.isPointer(theType)) {
      state.readVal(address.value, new CBasicType(IBasicType.Kind.eInt, 0))
    } else {
      state.readVal(address.value, TypeHelper.resolve(theType))
    }
  }

  def allocateSpace(state: State, aType: IType, numElements: Int): Address = {
    if (aType.isInstanceOf[IArrayType] || aType.isInstanceOf[IPointerType]) {
      val intType = new CBasicType(IBasicType.Kind.eInt, 0)
      state.allocateSpace(TypeHelper.sizeof(intType))
    } else if (aType.isInstanceOf[CStructure]) {
      val struct = aType.asInstanceOf[CStructure]
      var result: Address = Address(-1)
      struct.getFields.foreach { field =>
        if (result == Address(-1)) {
          result = allocateSpace(state, field.getType, numElements)
        } else {
          allocateSpace(state, field.getType, numElements)
        }
      }
      result
    } else if (aType.isInstanceOf[CTypedef]) {
      allocateSpace(state, aType.asInstanceOf[CTypedef].getType, numElements)
    } else {
      state.allocateSpace(TypeHelper.sizeof(aType) * numElements)
    }
  }
}

protected class ArrayVariable(val state: State, val theType: IType, dimensions: Seq[Int]) extends RuntimeVariable {

  val numElements = if (dimensions.isEmpty) 0 else dimensions.reduce{_ * _}

  // where we store the actual data
  val theArrayAddress = allocateSpace(state, TypeHelper.resolve(theType), numElements)

  // where we store the reference
  val address: Address = allocateSpace(state, theType, 1)

  state.setValue(theArrayAddress.value, info)

  def sizeof: Int = {
    TypeHelper.sizeof(theType) * numElements
  }

  def setValue(value: Any): Unit = value match {
    case array: Array[_] =>
      var i = 0
      array.foreach { element =>
        element match {
          case lit @ Literal(_) =>
            state.setValue(lit.cast, AddressInfo(theArrayAddress + i, theType))
          case int: Int =>
            state.setValue(int, AddressInfo(theArrayAddress + i, theType))
          case char: Char =>
            state.setValue(char, AddressInfo(theArrayAddress + i, theType))
          case double: Double =>
            state.setValue(double, AddressInfo(theArrayAddress + i, theType))
        }
        i += TypeHelper.sizeof(theType.asInstanceOf[IArrayType].getType)
      }
  }
}

protected class Variable(val state: State, val theType: IType) extends RuntimeVariable {

  val address: Address = allocateSpace(state, theType, 1)

  def sizeof: Int = {
    TypeHelper.sizeof(theType)
  }
}

class FunctionExecutionContext(globals: Map[String, RuntimeVariable]) {
  val visited = new ListBuffer[IASTNode]()
  val varMap = Map[String, RuntimeVariable]() ++ globals

  def resolveId(id: String) = varMap(id)
  def addVariable(id: String, theVar: RuntimeVariable) = varMap += (id -> theVar) 
}

object Executor {

  // 'node' must be a IASTCaseStatement or a IASTDefaultStatement
  def processSwitch(node: IASTNode): Seq[IASTNode] = {
    val codeToRun = new ListBuffer[IASTNode]()

    val siblings = node.getParent.getChildren

    var isSelfFound = false
    siblings.foreach { sib =>
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
      val cases = switch.getBody.getChildren.collect { case x: IASTCaseStatement => x; case y: IASTDefaultStatement => y }
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
          case int: Int  => int
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
          case x: Int     => x == 1
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
          case x                => x
        }

        val shouldLoop = cast match {
          case x: Int     => x == 1
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
          case x: Int     => x == 1
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
          case VarRef(id)       => state.vars.resolveId(id).value
          case int: Int         => int
          case doub: Double     => doub
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
  
  def resolve(state: State, theType: IType, any: Any): AnyVal = {
    any match {
      case VarRef(name) =>
        state.vars.resolveId(name).value
      case lit @ Literal(_) => lit.typeCast(TypeHelper.resolve(theType))
      case AddressInfo(addy, _) => addy.value
      case int: Int => int
      case float: Float => float
      case double: Double => double
      case long: Long => long
      case boolean: Boolean => boolean
    }
  }

  def parseDeclarator(decl: IASTDeclarator, direction: Direction, state: State): Seq[IASTNode] = {
    if (direction == Entering) {
      decl match {
        case array: IASTArrayDeclarator =>
          Seq(Option(decl.getInitializer)).flatten ++ array.getArrayModifiers
        case _ =>
          Seq(Option(decl.getInitializer)).flatten
      }
    } else {
      val nameBinding = decl.getName.resolveBinding()
      
      if (nameBinding.isInstanceOf[IVariable]) {
        val theType = nameBinding.asInstanceOf[IVariable].getType match {
          case typedef: CTypedef => typedef.getType
          case x                 => x
        }

        state.stack.push(decl.getName.getRawSignature)

        val name = state.stack.pop.asInstanceOf[String]

        decl match {
          case arrayDecl: IASTArrayDeclarator =>

            val dimensions = arrayDecl.getArrayModifiers.filter{_.getConstantExpression != null}.map{dim => state.stack.pop.asInstanceOf[Literal].cast.asInstanceOf[Int]}
            
            if (dimensions.isEmpty && decl.getInitializer != null) {
              // Deals with string initializers
              // e.g. char str[] = "Hello!\n";
              val initString = state.stack.pop.asInstanceOf[Literal].cast.asInstanceOf[StringLiteral].str
              val withNull = Utils.stripQuotes(initString).toCharArray() :+ 0.toChar // terminating null char
              val theArrayPtr = new ArrayVariable(state, theType, Seq(withNull.size))
              theArrayPtr.setValue(withNull)
              state.vars.addVariable(name, theArrayPtr)
            } else {
              val numElements = if (dimensions.isEmpty) 0 else dimensions.reduce{_ * _}
              val initialArray = Array.fill[Any](numElements)(0)

              if (!state.stack.isEmpty) {
                var i = 0
                for (i <- (numElements - 1) to 0 by -1) {
                  val newInit = state.stack.pop
                  initialArray(i) = newInit
                }
              }
              
              val theArrayPtr = new ArrayVariable(state, theType.asInstanceOf[IArrayType], dimensions)
              theArrayPtr.setValue(initialArray)
              state.vars.addVariable(name, theArrayPtr)
            }
          case decl: CASTDeclarator =>

            def createVariable(theType: IType, name: String): RuntimeVariable = theType match {
              case struct: CStructure =>
                val newStruct = new Variable(state, theType)
                state.vars.addVariable(name, newStruct)
                newStruct
              case typedef: CTypedef =>
                createVariable(typedef.getType, name)
              case _ =>
                
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
                
                val resolved = resolve(state, theType, initVal)
                val newVar = new Variable(state, theType)
                state.setValue(resolved, newVar.info)
                state.vars.addVariable(name, newVar)
                newVar
            }

            val newVar = createVariable(theType, name)
            
            if (decl.getInitializer != null && decl.getInitializer.isInstanceOf[IASTEqualsInitializer]
                 && decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause.isInstanceOf[IASTInitializerList]) {

              val fields = theType.asInstanceOf[CStructure].getFields
              val size = fields.size
              
              val values = fields.map{x => state.stack.pop.asInstanceOf[Literal].cast}.reverse zip fields

              var offset = 0
              values.foreach { case (value, field) =>
                state.setValue(value, AddressInfo(newVar.address + offset, theType))
                offset += TypeHelper.sizeof(field.getType)
              }
            }
        }

        Seq()
      } else {
        Seq()
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

          val name = param.getDeclarator.getName.getRawSignature
          val newVar = new Variable(state, paramInfo.getType)
          state.setValue(resolve(state, paramInfo.getType, arg), newVar.info)

          state.vars.addVariable(name, newVar)
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
          fcnDec.getChildren.filter(x => !x.isInstanceOf[IASTName]).map { x => x }
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
          val declsepc = typeId.getDeclSpecifier.asInstanceOf[IASTSimpleDeclSpecifier]
          val isPointer = typeId.getAbstractDeclarator.getPointerOperators.size > 1

          import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclSpecifier._
          
          val result = declsepc.getType match {
            case `t_int` if isPointer => new CPointerType(new CBasicType(IBasicType.Kind.eInt, 0), 0)
            case `t_int` if declsepc.isShort() => new CBasicType(IBasicType.Kind.eChar16, 0)
            case `t_int`    => new CBasicType(IBasicType.Kind.eInt, 0)
            case `t_float`  => new CBasicType(IBasicType.Kind.eFloat, 0)
            case `t_double` => new CBasicType(IBasicType.Kind.eDouble, 0)           
            case `t_char`   => new CBasicType(IBasicType.Kind.eChar, 0)
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

  engineState.executionContext.push(new FunctionExecutionContext(Map())) // load initial stack
  engineState.vars = engineState.executionContext.head

  execute()
  engineState.isPreprocessing = false
  engineState.stack.clear

  println("_----------------------------------------------_")

  engineState.globals ++= engineState.vars.varMap

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

    paths.reverse.foreach { path => pathStack.push(path) }

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
