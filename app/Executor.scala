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
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._

case class ValueInfo(value: AnyVal, theType: IType)
case class VarRef(name: String)

object Variable {                              
  def unapply(any: Any)(implicit state: State): Option[Any] = {
    if (any.isInstanceOf[VarRef]) {
      val ref = any.asInstanceOf[VarRef]
      if (state.context.containsId(ref.name)) {
        val resolved = state.context.resolveId(ref.name)
        Some(resolved)
      } else {
        None
      }
    } else if (any.isInstanceOf[Variable]) {
      Some(any)
    } else {
      None
    }
  }
  
  def allocateSpace(state: State, aType: IType, numElements: Int): Address = {
    if (aType.isInstanceOf[CFunctionType]) {
      state.allocateSpace(4 * numElements)
    } else if (TypeHelper.isPointer(aType)) {
      state.allocateSpace(TypeHelper.sizeof(TypeHelper.pointerType) * numElements)
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
    } else if (aType.isInstanceOf[IQualifierType]) {
      allocateSpace(state, aType.asInstanceOf[IQualifierType].getType, numElements)
    } else {
      state.allocateSpace(TypeHelper.sizeof(aType) * numElements)
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

// A symbolic reference is a string that becomes something else, payload: X, after processing
// For most variables, this is an address

trait SymbolicReference {
  val theType: IType
  def allocate: Address
}

protected class ArrayVariable(state: State, theType: IType, dim: Seq[Int]) extends Variable(state, theType) {
 
  override def allocate: Address = {
    // where we store the actual data
    
    val dimensions = dim.reverse
    
    if (!dimensions.isEmpty) {      
      address = Variable.allocateSpace(state, TypeHelper.pointerType, 1)
      
      def recurse(subType: IType, dimensions: Seq[Int]): Address = {

        val addr = Variable.allocateSpace(state, subType, dimensions.head)
        for (i <- (0 until dimensions.head)) {

          if (dimensions.size > 1) {
            val subaddr = recurse(subType, dimensions.tail)
            state.setValue(subaddr.value, addr + i * 4)
          }
        }
        addr
      }
      
      recurse(state.resolve(theType), dimensions)
    } else {
      Address(0)
    }
  }
  
  val theArrayAddress = allocate
  state.setValue(theArrayAddress.value, info.address)

  def setArray(array: Array[_])(implicit state: State): Unit = {
      state.setArray(array, AddressInfo(theArrayAddress, theType))
  }
  
  override def sizeof: Int = {
    val numElements = if (dim.isEmpty) 0 else dim.reduce{_ * _}
    TypeHelper.sizeof(theType) * numElements
  }
}

protected class Variable(val state: State, val theType: IType) extends SymbolicReference {
  var address = Address(0)
  
  def allocate: Address = {
    address = Variable.allocateSpace(state, theType, 1)
    address
  }
  
  val size = TypeHelper.sizeof(theType)

  def info = AddressInfo(address, theType)
  
  def setValues(values: List[ValueInfo]) = {
     var offset = 0
      values.foreach { case ValueInfo(value, theType) =>
        state.setValue(value, address + offset)
        offset += TypeHelper.sizeof(theType)
      }
  }

  
  
  def value: ValueInfo = {
    ValueInfo(state.readVal(address, theType).value, theType)
  }
  
  def sizeof: Int = {
    TypeHelper.sizeof(theType)
  }
}

class ExecutionContext(parentVars: Map[String, Variable], val returnType: IType, state: State) {
  val visited = new ListBuffer[IASTNode]()
  val varMap = parentVars.clone()
  val pathStack = new Stack[IASTNode]()
  val stack = new Stack[Any]()

  def containsId(id: String) = varMap.contains(id) || state.functionPointers.contains(id)
  def resolveId(id: String): Variable = varMap.get(id).getOrElse(state.functionPointers(id))
  def addVariable(id: String, theVar: Variable) = varMap += (id -> theVar) 
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
        
        val resolved = TypeHelper.resolve(switchExpr).value

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
        val shouldLoop = TypeHelper.resolveBoolean(state.stack.pop)

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

        val shouldLoop = TypeHelper.resolveBoolean(cast)

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
          case Variable(info: Variable) => info.value
          case lit @ Literal(_) =>
            lit.cast.value
          case x => x
        }

        val conditionResult = TypeHelper.resolveBoolean(value)
        
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
          
          val result = TypeHelper.resolve(state.stack.pop).value
          
          TypeHelper.resolveBoolean(result)
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
            case lit @ Literal(_) if state.context.returnType != null => lit.typeCast(TypeHelper.resolve(state.context.returnType))
            case lit @ Literal(_) => lit.cast.value
            case Variable(info: Variable)       => 
              if (TypeHelper.isPointer(state.context.returnType)) {
                info.value.value
              } else {
                TypeHelper.cast(state.context.returnType, info.value.value)
              }
            case addy @ AddressInfo(addr, theType) => addy
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
              case Variable(info: Variable) => info.value.value.asInstanceOf[Int]
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
                theArrayPtr.allocate
                state.setValue(strAddr.value, theArrayPtr.address)
                state.context.addVariable(name, theArrayPtr)
              } else {
                val list = initializer.getInitializerClause.asInstanceOf[IASTInitializerList]
                val size = list.getSize
                
                val values: Array[Any] = (0 until size).map{x => state.stack.pop match {
                  case lit: Literal => lit.cast
                  case int: Int => int
                  case ValueInfo(value,_) => value
                  case Variable(theVar: Variable) => theVar.value
                }}.reverse.toArray
  
                val theArrayPtr = new ArrayVariable(state, theType.asInstanceOf[IArrayType], Array(size))

                theArrayPtr.setArray(values.toArray)
                state.context.addVariable(name, theArrayPtr)
              }
            } else if (initializer != null) {
              val initVals: Array[Any] = (0 until initializer.getInitializerClause.getChildren.size).map{x => state.stack.pop}.reverse.toArray
              
              val theArrayVar: ArrayVariable = new ArrayVariable(state, theType.asInstanceOf[IArrayType], dimensions)
              
              var resolvedType = theArrayVar.info.theType
              
              val initialArray = initVals.map { newInit =>
                if (newInit.isInstanceOf[StringLiteral]) {
                  val result = state.createStringVariable(newInit.asInstanceOf[StringLiteral].str)
                  resolvedType = TypeHelper.pointerType
                  result
                } else {
                  (newInit match {
                    case Variable(x: Variable) => x.value
                    case x => x
                  })
                }
              }

              state.setArray(initialArray.toArray, AddressInfo(theArrayVar.theArrayAddress, resolvedType))
              state.context.addVariable(name, theArrayVar)
            } else {
              val initialArray = new ListBuffer[Any]()            
              val theArrayVar = new ArrayVariable(state, theType.asInstanceOf[IArrayType], dimensions)             
              state.context.addVariable(name, theArrayVar)
            }
          case decl: CASTDeclarator =>

            def createVariable(theType: IType, name: String): Variable = theType match {
              case struct: CStructure =>
                val newStruct = new Variable(state, theType)
                newStruct.allocate
                state.context.addVariable(name, newStruct)
                newStruct
              case typedef: CTypedef =>
                createVariable(typedef.getType, name)
              case qual: CQualifierType =>
                createVariable(qual.getType, name)
              case ptr: IPointerType =>
                val initVal = Option(decl.getInitializer).map(x => state.stack.pop).getOrElse(0)
                
                val newVar = initVal match {
                  case Variable(info: Variable) => 
                    val newVar = new Variable(state, theType)
                    newVar.allocate
                    state.setValue(info.value.value, newVar.address)
                    newVar
                  case AddressInfo(address, addrType) => 
                    val newVar = new Variable(state, theType)
                    newVar.allocate
                    if (TypeHelper.isPointer(addrType)) {
                      state.setValue(state.readVal(address, TypeHelper.pointerType).value, newVar.address)
                    } else {
                      state.setValue(address.value, newVar.address)
                    }
                    newVar
                  case int: Int => 
                    val newVar = new Variable(state, theType)
                    newVar.allocate
                    state.setValue(int, newVar.address)
                    newVar
                  case prim @ ValueInfo(_, newType) => 
                    
                    val newVar = new Variable(state, theType)
                    newVar.allocate
                    state.setValue(prim.value, newVar.address)
                    newVar
                  case StringLiteral(str) =>
                    val newVar = new Variable(state, theType)
                    newVar.allocate
                    val strAddr = state.createStringVariable(str)
                    state.setValue(strAddr.value, newVar.address)
                    newVar
                  case lit @ Literal(_) => 
                    val newVar = new Variable(state, theType) // TODO: Not setting value? Whats going on here?
                    newVar.allocate
                    newVar
                  case Address(int) => 
                    val newVar = new Variable(state, theType)
                    newVar.allocate
                    state.setValue(int, newVar.address)
                    newVar
                }
                
                state.context.addVariable(name, newVar)
                newVar               
               
              case basic: IBasicType =>
                val initVal = Option(decl.getInitializer).map(x => state.stack.pop).getOrElse(0)   

                val newVar = new Variable(state, theType)
                newVar.allocate
                state.context.addVariable(name, newVar)

                BinaryExpr.parseAssign(op_assign, newVar, initVal)

                newVar
            }

            val newVar = createVariable(theType, name)
            
            if (decl.getInitializer != null && decl.getInitializer.isInstanceOf[IASTEqualsInitializer]
                 && decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause.isInstanceOf[IASTInitializerList]) {

              val fields = theType.asInstanceOf[CStructure].getFields
              val size = fields.size
              
              val clause = decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause
              println(clause.getClass.getSimpleName)
              
              clause match {
                case list: IASTInitializerList =>
                  
                  val values: Array[(ValueInfo, IType)] = if (list.getClauses.size == 1 && list.getClauses.head.getRawSignature == "0") {
                    fields.map{ field =>
                      ValueInfo(0, field.getType)
                    }.reverse zip fields.map(_.getType)
                  } else {
                    list.getClauses.map{x => state.stack.pop match {
                      case lit: Literal => lit.cast
                      case Address(value) => ValueInfo(value, new CBasicType(IBasicType.Kind.eInt, 4))
                    }}.reverse zip fields.map(_.getType)
                  }
                  
                  val valueInfos = values.map{x => ValueInfo(x._1.value, x._2)}.toList
                  newVar.setValues(valueInfos)
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
      case tUnit: IASTTranslationUnit =>
        if (direction == Entering) {
          tUnit.getChildren.filterNot{_.isInstanceOf[IASTFunctionDefinition]}
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
        val isInFunctionPrototype = Utils.getAncestors(fcnDec).exists{_.isInstanceOf[IASTSimpleDeclaration]}
          
        // ignore main's params for now
        val isInMain = fcnDec.getName.getRawSignature == "main"
        val fcnName = fcnDec.getName.getRawSignature
        
        val paramDecls = new Stack[IASTParameterDeclaration]() ++ fcnDec.getChildren.collect{case x: IASTParameterDeclaration => x}
        
        if (!paramDecls.isEmpty && direction == Entering && !isInMain) {
          
          var argAddress = Address(0)
          var fcnCall: IASTFunctionCallExpression = null   
          
          val others = fcnDec.getChildren.filter{x => !x.isInstanceOf[IASTParameterDeclaration] && !x.isInstanceOf[IASTName]}
          
          if (!isInFunctionPrototype) {
            argAddress = state.stack.pop.asInstanceOf[Address]
            fcnCall = state.stack.pop.asInstanceOf[IASTFunctionCallExpression]
          
          fcnCall.getArguments.foreach{ param =>
            if (!isInFunctionPrototype && !paramDecls.isEmpty) {
              
              val paramDecl = paramDecls.pop
              
              val paramInfo = paramDecl.getDeclarator.getName.resolveBinding().asInstanceOf[CParameter]
              
              val function = state.getFunction(fcnName)
  
              val arg = state.readVal(argAddress, paramInfo.getType)
              
              argAddress += TypeHelper.sizeof(paramInfo.getType)
    
              val name = paramDecl.getDeclarator.getName.getRawSignature
              val newVar = new Variable(state, paramInfo.getType)
              newVar.allocate
              val resolved = TypeHelper.resolve(arg).value
              val casted = TypeHelper.cast(newVar.info.theType, resolved).value
              state.setValue(casted, newVar.info.address)          
          
              state.context.addVariable(name, newVar)
            } else if (state.context.containsId(param.getRawSignature)) {
              val theVar = state.context.resolveId(param.getRawSignature)
              val sizeof = TypeHelper.sizeof(theVar.theType)
              val space = state.allocateSpace(sizeof)
              val arg = state.readVal(argAddress, theVar.theType).value
              argAddress += sizeof
              val casted = TypeHelper.cast(theVar.theType, arg).value
              state.setValue(casted, space) 
            } else if  (paramDecls.isEmpty) {
              val lit = Literal(param.getRawSignature).cast              
              val inferredType = lit.theType
              val sizeof = TypeHelper.sizeof(inferredType)
              val space = state.allocateSpace(4)
              val arg = state.readVal(argAddress, inferredType).value
              argAddress += sizeof
              val casted = TypeHelper.cast(inferredType, arg).value
              state.setValue(casted, space)     
            }
          }
        }
          
          others
        } else {
          Seq()
        }
      case decl: IASTDeclarator =>
        Executor.parseDeclarator(decl, direction)
      case fcnDef: IASTFunctionDefinition =>
        if (direction == Exiting) {
          if (!state.context.stack.isEmpty) {
            val retVal = state.context.stack.pop
            if (fcnDef.getDeclarator.getName.getRawSignature != "main") {
              state.functionContexts.pop
            }
            state.context.stack.push(retVal)
          } else {
            if (fcnDef.getDeclarator.getName.getRawSignature != "main") {
              state.functionContexts.pop
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

              var config = 0
              
              if (simple.isLong) {
                config |= IBasicType.IS_LONG
              }
              
              if (simple.isUnsigned) {
                config |= IBasicType.IS_UNSIGNED
              } else {
                config |= IBasicType.IS_SIGNED
              }
              
              var result: IType = simple.getType match {
                case `t_unspecified` => new CBasicType(IBasicType.Kind.eInt, config)
                case `t_int`    => new CBasicType(IBasicType.Kind.eInt, config)
                case `t_float`  => new CBasicType(IBasicType.Kind.eFloat, config)
                case `t_double` => new CBasicType(IBasicType.Kind.eDouble, config)           
                case `t_char`   => new CBasicType(IBasicType.Kind.eChar, config)
                case `t_void`   => new CBasicType(IBasicType.Kind.eVoid, config)
                case `t_typeof`   => new CBasicType(IBasicType.Kind.eVoid, config) // FIX
              }
              
              for (ptr <- typeId.getAbstractDeclarator.getPointerOperators) {
                result = new CPointerType(result, 0)
              }
              
              result
              
            case typespec: CASTTypedefNameSpecifier =>
              val name: IASTName = typespec.getName
              val defs = Executor.tUnit.getDefinitionsInAST(name.getBinding)
              if (!defs.isEmpty) {
                val typedef = defs.head.resolveBinding() match {
                  case x: CTypedef => x.getType
                  case null => typespec.getName.resolveBinding().asInstanceOf[CTypedef].getType
                }
                typedef
              } else {
                null
              }
            case elab: CASTElaboratedTypeSpecifier =>
              elab.getName.resolveBinding().asInstanceOf[CStructure]
          }

          state.stack.push(result)
        }
        Seq()
    }
  }
  
  var tUnit: IASTTranslationUnit = null
  
  def init(codes: Seq[String], reset: Boolean, state: State) = {
    tUnit = Utils.getTranslationUnit(codes)
    current = tUnit

    if (reset) {
      state.functionContexts.push(new ExecutionContext(Map(), null, state)) // load initial stack
    }

    val fcns = tUnit.getChildren.collect{case x:IASTFunctionDefinition => x}.filter(_.getDeclSpecifier.getStorageClass != IASTDeclSpecifier.sc_extern)
    val (mainFcn, others) = fcns.partition{fcnDef => fcnDef.getDeclarator.getName.getRawSignature == "main"}
    others.foreach{fcnDef => state.addFunctionDef(fcnDef)}
    
    run(state)

    state.context.pathStack.clear
    state.context.pathStack.push(mainFcn.head)
    current = state.context.pathStack.head
  }

  var current: IASTNode = null
  var direction: Direction = Entering

  def tick(engineState: State): Unit = {
    direction = if (engineState.context.visited.contains(current)) Exiting else Entering

    //println(current.getClass.getSimpleName + ":" + direction)
    
    var paths: Seq[IASTNode] = Executor.step(current, direction)(engineState)
    
    if (engineState.isBreaking) {
      // unroll the path stack until we meet the first parent which is a loop
      
      val breakStatement = engineState.context.pathStack.pop.asInstanceOf[CASTBreakStatement]
      var reverse: IASTNode = breakStatement
      while ((!reverse.isInstanceOf[IASTWhileStatement] &&
          !reverse.isInstanceOf[IASTDoStatement] &&
          !reverse.isInstanceOf[IASTForStatement] &&
          !reverse.isInstanceOf[IASTSwitchStatement]) || !Utils.getAncestors(breakStatement).contains(reverse)) {
        reverse = engineState.context.pathStack.pop
      }

      engineState.isBreaking = false
    }


    if (engineState.isContinuing) {
      // unroll the path stack until we meet the first parent which is a loop

      val continueStatement = engineState.context.pathStack.pop.asInstanceOf[CASTContinueStatement]
      var last: IASTNode = continueStatement

      // find the first for loop that is a direct ancestor
      while (!last.isInstanceOf[IASTForStatement] || !Utils.getAncestors(continueStatement).contains(last)) {
        last = engineState.context.pathStack.pop
      }

      val forLoop = last.asInstanceOf[IASTForStatement]

      engineState.context.pathStack.push(forLoop)
      engineState.context.pathStack.push(forLoop.getConditionExpression)
      engineState.context.pathStack.push(forLoop.getIterationExpression)

      engineState.isContinuing = false
    }
    
    if (engineState.isReturning) {
      var last: IASTNode = null
      while (engineState.context.pathStack.size > 1 && !last.isInstanceOf[IASTFunctionDefinition]) {
        last = engineState.context.pathStack.pop
      }

      current = engineState.context.pathStack.head
      engineState.isReturning = false
    } else {

      if (direction == Exiting) {
        engineState.context.pathStack.pop
      } else {
        engineState.context.visited += current
      }
  
      paths.reverse.foreach { path => engineState.context.pathStack.push(path) }
  
      if (!engineState.context.pathStack.isEmpty) {
        current = engineState.context.pathStack.head
      } else {
        current = null
      }
    }
  }

  def run(engineState: State) = {
    while (current != null) {
      tick(engineState)
    }
  }
}
