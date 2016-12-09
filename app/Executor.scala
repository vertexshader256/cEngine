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

case class ValueInfo(value: AnyVal, theType: IType)
case class VarRef(name: String)

object Variable {                              
  def apply(x: Int): Int = x * 2
  def unapply(any: Any)(implicit state: State): Option[(ValueInfo, AddressInfo)] = {
    if (any.isInstanceOf[VarRef]) {
      val ref = any.asInstanceOf[VarRef]
      val vari = state.vars.resolveId(ref.name)
      Some((vari.value, vari.info))
    } else {
      None
    }
  }
}

case class StringLiteral(str: String) extends AnyVal



case class Address(value: Int) extends AnyVal {
  def +(x: Int) = {
    Address(value + x)
  }
}
case class AddressInfo(address: Address, theType: IType)

trait RuntimeVariable {
  val state: State
  val theType: IType
  def address: Address
  
  val size = TypeHelper.sizeof(theType)

  def sizeof: Int
  def info = AddressInfo(address, theType)
  
  def value: ValueInfo = {
    ValueInfo(state.readVal(address, theType).value, theType)
  }
  
  def pointerValue: Int = {
    state.readVal(address, TypeHelper.pointerType).value.asInstanceOf[Int]
  }

  def allocateSpace(state: State, aType: IType, numElements: Int): Address = {
    if (TypeHelper.isPointer(aType)) {
      state.allocateSpace(TypeHelper.sizeof(TypeHelper.pointerType))
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

protected class ArrayVariable(val state: State, val theType: IType, dim: Seq[Int]) extends RuntimeVariable {

  val dimensions = dim.reverse
  
  val numElements = if (dimensions.isEmpty) 0 else dimensions.reduce{_ * _}

  val address: Address = allocateSpace(state, TypeHelper.pointerType, 1)
  
  
  
  
  def allocateData(subType: IType, dimensions: Seq[Int]): Address = {
    // where we store the actual data

      val addr = allocateSpace(state, TypeHelper.pointerType, dimensions.head)
      for (i <- (0 until dimensions.head)) {
        
        val sub = state.resolve(subType)

        if (dimensions.size > 1) {
          val subaddr = allocateData(sub, dimensions.tail)
          state.setValue(subaddr.value, addr + i * 4)
        }
      }
      addr
  }
  
  val theArrayAddress = if (!dimensions.isEmpty) {    
    val sub = state.resolve(theType)  
    allocateData(sub, dimensions)
  } else {
    Address(0)
  }

  state.setValue(theArrayAddress.value, info.address)

  def sizeof: Int = {
    TypeHelper.sizeof(theType) * numElements
  }
}

protected class Variable(val state: State, val theType: IType) extends RuntimeVariable {

  val address: Address = allocateSpace(state, theType, 1)

  def sizeof: Int = {
    TypeHelper.sizeof(theType)
  }
}

class FunctionExecutionContext(globals: Map[String, RuntimeVariable], val returnType: IType) {
  val visited = new ListBuffer[IASTNode]()
  val varMap = Map[String, RuntimeVariable]() ++ globals
  val pathStack = new Stack[IASTNode]()
  val stack = new Stack[Any]()

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

  def parseStatement(statement: IASTStatement, direction: Direction)(implicit state: State): Seq[IASTNode] = statement match {
    case breakStatement: IASTNullStatement =>
      Seq()
    case breakStatement: IASTBreakStatement =>
      state.isBreaking = true
      Seq()
    case continueStatement: IASTContinueStatement =>
      state.isContinuing = true
      Seq()
    case label: IASTLabelStatement =>
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

        val caseExpr = state.stack.pop.asInstanceOf[Literal].cast.value
        val switchExpr = state.stack.head
        
        val resolved = TypeHelper.resolve(switchExpr)

        if (caseExpr == resolved) {
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
          case x: Boolean => x
          case int: Int => int > 0
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
          case lit @ Literal(_) => lit.cast.value
          case x                => x
        }

        val shouldLoop = cast match {
          case ValueInfo(x: Int,_)     => x > 0
          case ValueInfo(x: Character,_)     => x > 0
          case x: Int     => x > 0
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
          case Variable(value, _) => value
          case lit @ Literal(_) =>
            lit.cast.value
          case x => x
        }

        val conditionResult = value match {
          case x: Int     => x > 0
          case ValueInfo(x: Int, _)     => x > 0
          case ValueInfo(x: Character, _)     => x > 0
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
        Seq(Option(forLoop.getInitializerStatement), Option(forLoop.getConditionExpression)).flatten
      } else {
        val shouldKeepLooping = if (forLoop.getConditionExpression != null) {
          
          val result = TypeHelper.resolve(new CBasicType(IBasicType.Kind.eInt, 0), state.stack.pop).value
          
          result match {
            case bool: Boolean => bool
            case int: Int => int > 0
            case char: Character => char > 0
          }
        } else {
          true
        }

        if (shouldKeepLooping) {
          state.clearVisited(forLoop.getBody)
          state.clearVisited(forLoop.getIterationExpression)
          
          if (forLoop.getConditionExpression != null) {
            state.clearVisited(forLoop.getConditionExpression)
          }

          Seq(Option(forLoop.getBody), Option(forLoop.getIterationExpression), Option(forLoop.getConditionExpression), Some(forLoop)).flatten
        } else {
          Seq()
        }
      }
    case ret: IASTReturnStatement =>
      if (direction == Entering) {
        if (ret.getReturnValue != null) {
          Seq(ret.getReturnValue)
        } else {
          Seq()
        }
      } else {
        // resolve everything before returning
        if (ret.getReturnValue != null) {
          val returnVal = state.stack.pop
          state.stack.push(returnVal match {
            case lit @ Literal(_) if state.vars.returnType != null => lit.typeCast(TypeHelper.resolve(state.vars.returnType))
            case lit @ Literal(_) => lit.cast.value
            case VarRef(id)       => 
              if (TypeHelper.isPointer(state.vars.returnType)) {
                state.vars.resolveId(id).pointerValue
              } else {
                TypeHelper.cast(state.vars.returnType, state.vars.resolveId(id).value.value)
              }
            case ValueInfo(theVal, _) => theVal
            case int: Int         => int
            case doub: Double     => doub
            case bool: Boolean    => bool
          })
        }
        state.isReturning = true
        
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
  
  def parseDeclarator(decl: IASTDeclarator, direction: Direction)(implicit state: State): Seq[IASTNode] = {
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

            val dimensions = arrayDecl.getArrayModifiers.filter{_.getConstantExpression != null}.map{dim => state.stack.pop match {
              // can we can assume dimensions are integers
              case lit: Literal => lit.cast.value.asInstanceOf[Int]
              case Variable(value, _) => value.value.asInstanceOf[Int]
              case int: Int => int
            }}
            
            val initializer = decl.getInitializer.asInstanceOf[IASTEqualsInitializer]
            
            // Oddly enough, it is possible to have a pointer to an array with no dimensions OR initializer:
            //    extern char *x[]
            
            if (dimensions.isEmpty && initializer != null) {       
              
              if (TypeHelper.resolve(theType).getKind == eChar && !initializer.getInitializerClause.isInstanceOf[IASTInitializerList]) {
                // char str[] = "Hello!\n";
                val initString = state.stack.pop.asInstanceOf[StringLiteral].str                
                val strAddr = state.createStringVariable(initString)
                val theArrayPtr = new Variable(state, theType.asInstanceOf[IArrayType])
                state.setValue(strAddr.value, theArrayPtr.address)
                state.vars.addVariable(name, theArrayPtr)
              } else {
                val list = initializer.getInitializerClause.asInstanceOf[IASTInitializerList]
                val size = list.getSize
                
                val values = (0 until size).map{x => state.stack.pop match {
                  case lit: Literal => lit.cast
                  case int: Int => int
                }}.reverse
  
                val theArrayPtr = new ArrayVariable(state, theType.asInstanceOf[IArrayType], Array(size))
                state.setArray(values.toArray, AddressInfo(theArrayPtr.theArrayAddress, theArrayPtr.info.theType))
                state.vars.addVariable(name, theArrayPtr)
              }
            } else if (initializer != null) {
              val numElements = if (dimensions.isEmpty) 0 else dimensions.reduce{_ * _}
              val initialArray = new ListBuffer[Any]()
              
              val theArrayPtr: ArrayVariable = new ArrayVariable(state, theType.asInstanceOf[IArrayType], dimensions)

              val initVals = (0 until initializer.getInitializerClause.getChildren.size).map{x => state.stack.pop}.reverse
              
              initVals.foreach { newInit =>
                if (newInit.isInstanceOf[StringLiteral]) {
                  initialArray += state.createStringVariable(newInit.asInstanceOf[StringLiteral].str)
                } else {
                  initialArray += newInit
                }
              }
              
              val resolvedType = if (initialArray.head.isInstanceOf[Address]) {
                TypeHelper.pointerType
              } else {
                theArrayPtr.info.theType
              }
              
              state.setArray(initialArray.toArray, AddressInfo(theArrayPtr.theArrayAddress, resolvedType))
              state.vars.addVariable(name, theArrayPtr)
            } else {
              val initialArray = new ListBuffer[Any]()            
              val theArrayPtr = new ArrayVariable(state, theType.asInstanceOf[IArrayType], dimensions)             
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
              case qual: CQualifierType =>
                createVariable(qual.getType, name)
              case ptr: IPointerType =>
                val initVal = Option(decl.getInitializer).map(x => state.stack.pop).getOrElse(0)
                
                val newVar = initVal match {
                  case Variable(value, info) => 
                    val newVar = new Variable(state, theType)
                    state.setValue(value.value, newVar.address)
                    newVar
                  case AddressInfo(address, addrType) => 
                    val newVar = new Variable(state, theType)
                    if (TypeHelper.isPointer(addrType)) {
                      state.setValue(state.readVal(address, TypeHelper.pointerType).value, newVar.address)
                    } else {
                      state.setValue(address.value, newVar.address)
                    }
                    newVar
                  case int: Int => 
                    val newVar = new Variable(state, theType)
                    state.setValue(int, newVar.address)
                    newVar
                  case prim @ ValueInfo(_, newType) => 
                    val newVar = new Variable(state, theType)
                    state.setValue(prim.value, newVar.address)
                    newVar
                  case StringLiteral(str) =>
                    val newVar = new Variable(state, theType)
                    val strAddr = state.createStringVariable(str)
                    state.setValue(strAddr.value, newVar.address)
                    newVar
                  case lit @ Literal(_) => 
                    val newVar = new Variable(state, theType) // TODO: Not setting value? Whats going on here?
                    newVar
                  case Address(int) => 
                    val newVar = new Variable(state, theType) // TODO: Not setting value? Whats going on here?
                    newVar
                }
                
                state.vars.addVariable(name, newVar)
                newVar               
               
              case basic: IBasicType =>
                val initVal = Option(decl.getInitializer).map(x => state.stack.pop).getOrElse(0)   

                val resolved = TypeHelper.resolve(theType, initVal).value

                val newVar = new Variable(state, theType)
                val casted = TypeHelper.cast(newVar.info.theType, resolved).value
                state.setValue(casted, newVar.info.address)
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
                state.setValue(value.value, newVar.address + offset)
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

  def step(current: IASTNode, direction: Direction)(implicit state: State): Seq[IASTNode] = {

    current match {
      case statement: IASTStatement =>
        Executor.parseStatement(statement, direction)
      case expression: IASTExpression =>
        Expressions.parse(expression, direction)
      case array: IASTArrayModifier =>
        if (direction == Exiting) {
          Seq()
        } else {
          if (array.getConstantExpression != null) {
            Seq(array.getConstantExpression)
          } else {
            Seq()
          }
        }
      case ptr: IASTPointer =>
        Seq()
      case param: IASTParameterDeclaration =>
        if (direction == Exiting && !state.isPreprocessing) {
          val paramInfo = param.getDeclarator.getName.resolveBinding().asInstanceOf[CParameter]
          
          if (!paramInfo.getType.isInstanceOf[IBasicType] || 
              paramInfo.getType.asInstanceOf[IBasicType].getKind != eVoid) {
            val arg = state.stack.pop
  
            val name = param.getDeclarator.getName.getRawSignature
            val newVar = new Variable(state, paramInfo.getType)
            
            val resolved = TypeHelper.resolve(paramInfo.getType, arg).value
            val casted = TypeHelper.cast(newVar.info.theType, resolved).value
            state.setValue(casted, newVar.info.address)          
        
            state.vars.addVariable(name, newVar)
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
        Executor.parseDeclarator(decl, direction)
      case fcnDef: IASTFunctionDefinition =>
        if (state.isPreprocessing) {
          state.functionMap += (fcnDef.getDeclarator.getName.getRawSignature -> fcnDef)
          Seq()
        } else if (direction == Exiting) {
          if (!state.vars.stack.isEmpty) {
            val retVal = state.vars.stack.head
            if (fcnDef.getDeclarator.getName.getRawSignature != "main") {
              state.functionContext.pop
            }
            state.vars.stack.push(retVal)
          } else {
            if (fcnDef.getDeclarator.getName.getRawSignature != "main") {
              state.functionContext.pop
            }
          }
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
          val result = typeId.getDeclSpecifier match {
            case simple: IASTSimpleDeclSpecifier =>
              import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclSpecifier._
              
              simple.getType match {
                case `t_int`    => new CBasicType(IBasicType.Kind.eInt, 0)
                case `t_float`  => new CBasicType(IBasicType.Kind.eFloat, 0)
                case `t_double` => new CBasicType(IBasicType.Kind.eDouble, 0)           
                case `t_char`   => new CBasicType(IBasicType.Kind.eChar, 0)
                case `t_void`   => new CBasicType(IBasicType.Kind.eVoid, 0)
              }
            case simple: CASTTypedefNameSpecifier =>
              null
            case elab: CASTElaboratedTypeSpecifier =>
              elab.getName.resolveBinding().asInstanceOf[CStructure]
          }

          state.stack.push(result)
        }
        Seq()
    }
  }
}

class Executor() {
  
  var tUnit: IASTNode = null
  
  def init(code: String, reset: Boolean, engineState: State) = {
    tUnit = Utils.getTranslationUnit(code)
    current = tUnit

    engineState.functionMap.remove("main")
    engineState.functionContext.push(new FunctionExecutionContext(Map(), null)) // load initial stack
  
    engineState.isPreprocessing = true
    execute(engineState)
    engineState.isPreprocessing = false
    engineState.stack.clear
  
    println("_----------------------------------------------_")
  
    engineState.globals ++= engineState.vars.varMap
  
    engineState.functionContext.pop
    if (reset) {
      engineState.functionContext.push(new FunctionExecutionContext(engineState.globals, null)) // load initial stack
    }

    engineState.vars.pathStack.clear
    engineState.vars.pathStack.push(engineState.functionMap("main"))
    current = engineState.vars.pathStack.head
  }

  
  
  var current: IASTNode = null
  var direction: Direction = Entering

  

  def tick(engineState: State): Unit = {
    direction = if (engineState.vars.visited.contains(current)) Exiting else Entering

    //println(current.getClass.getSimpleName + ":" + direction)
    
    var paths: Seq[IASTNode] = Executor.step(current, direction)(engineState)
    
    if (engineState.isBreaking) {
      // unroll the path stack until we meet the first parent which is a loop
      var reverse = engineState.vars.pathStack.pop
      while (!reverse.isInstanceOf[IASTWhileStatement] && !reverse.isInstanceOf[IASTForStatement] && !reverse.isInstanceOf[IASTSwitchStatement]) {
        reverse = engineState.vars.pathStack.pop
      }

      engineState.isBreaking = false
    }


    if (engineState.isContinuing) {
      // unroll the path stack until we meet the first parent which is a loop

      var last: IASTNode = null
      last = engineState.vars.pathStack.pop
      while (!last.isInstanceOf[IASTForStatement]) {
        last = engineState.vars.pathStack.pop
      }

      val forLoop = last.asInstanceOf[IASTForStatement]

      engineState.vars.pathStack.push(forLoop)
      engineState.vars.pathStack.push(forLoop.getConditionExpression)
      engineState.vars.pathStack.push(forLoop.getIterationExpression)

      engineState.isContinuing = false
    }
    
    if (engineState.isReturning) {
      var last: IASTNode = null
      while (engineState.vars.pathStack.size > 1 && !last.isInstanceOf[IASTFunctionDefinition]) {
        last = engineState.vars.pathStack.pop
      }

      current = engineState.vars.pathStack.head
      engineState.isReturning = false
    } else {

      if (direction == Exiting) {
        engineState.vars.pathStack.pop
      } else {
        engineState.vars.visited += current
      }
  
      paths.reverse.foreach { path => engineState.vars.pathStack.push(path) }
  
      if (!engineState.vars.pathStack.isEmpty) {
        current = engineState.vars.pathStack.head
      } else {
        current = null
      }
    }
  }

  def execute(engineState: State) = {
    while (current != null) {
      tick(engineState)
    }
  }
}
