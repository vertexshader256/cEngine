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

object Variable {                              

  def allocateSpace(state: State, aType: IType, numElements: Int): Int = {
    aType match {
      case array: IArrayType =>
        state.allocateSpace(TypeHelper.sizeof(TypeHelper.pointerType) * numElements)
      case array: IPointerType =>
        state.allocateSpace(TypeHelper.sizeof(TypeHelper.pointerType) * numElements)
      case fcn: CFunctionType =>
        state.allocateSpace(TypeHelper.sizeof(TypeHelper.pointerType) * numElements)
      case structType: CStructure =>
        val struct = structType.asInstanceOf[CStructure]
        var result = -1
        struct.getFields.foreach { field =>
          if (result == -1) {
            result = allocateSpace(state, field.getType, numElements)
          } else {
            allocateSpace(state, field.getType, numElements)
          }
        }
        result
      case typedef: CTypedef =>
        allocateSpace(state, typedef.asInstanceOf[CTypedef].getType, numElements)
      case qual: IQualifierType =>
        allocateSpace(state, qual.asInstanceOf[IQualifierType].getType, numElements)
      case basic: IBasicType =>
        state.allocateSpace(TypeHelper.sizeof(basic) * numElements)
//      } else {
//        state.allocateSpace(TypeHelper.sizeof(aType) * numElements)
//      }
    }
  }
}

trait Stackable {
  def value: Any
}

case class ValueInfo(value: AnyVal, theType: IType) extends Stackable

object ValueInfo2 {
  def apply(value: AnyVal, theType: IType)(implicit state: State): AddressInfo = {
    val addr = state.allocateSpace(TypeHelper.sizeof(TypeHelper.getType(value)))
    state.setValue(value, addr)
    AddressInfo(addr, theType)
  }
}

case class StringLiteral(value: String) extends Stackable
case class TypeInfo(value: IType) extends Stackable

object AddressInfo {
  def unapply(info: AddressInfo): Option[(Int, IType)] = Some((info.address, info.theType))

  def apply(addr: Int, aType: IType)(implicit state: State) = {
    new AddressInfo(state) {
      val address = addr
      val theType = aType
    }
  }
}

abstract class AddressInfo(state: State) extends Stackable {
  val address: Int
  val theType: IType
  def sizeof = TypeHelper.sizeof(theType)
  def value: ValueInfo = state.readVal(address, theType)
}

protected class ArrayVariable(state: State, arrayType: IArrayType, dim: Seq[Int]) extends Variable(state, arrayType) {

  override val theType = arrayType
  override val address = Variable.allocateSpace(state, theType, 1)

  val allocate: Int = {
    // where we store the actual data

    def recurse(subType: IArrayType, dimensions: Seq[Int]): Int = {

      if (dimensions.size > 0) {
        val addr = Variable.allocateSpace(state, subType.getType, dimensions.head)
        for (i <- (0 until dimensions.head)) {
          if (dimensions.size > 1) {
            val subaddr = recurse(subType.getType.asInstanceOf[IArrayType], dimensions.tail)
            state.setValue(subaddr, addr + i * 4)
          }
        }
        addr
      } else {
        0
      }
    }

    recurse(theType, dim.reverse)
  }

  state.setValue(address + 4, address)

  def setArray(array: Array[ValueInfo])(implicit state: State): Unit = {
      state.setArray(array, AddressInfo(address + 4, theType))
  }
  
  override def sizeof: Int = {
    val numElements = if (dim.isEmpty) 0 else dim.reduce{_ * _}
    TypeHelper.sizeof(theType) * numElements
  }
}

protected class Variable(val state: State, aType: IType) extends AddressInfo(state) {
  val theType = aType
  val size = TypeHelper.sizeof(aType)
  val address = Variable.allocateSpace(state, aType, 1)

  def setValues(values: List[Stackable]) = {
     var offset = 0
      values.foreach {
        case ValueInfo(value, theType) =>
          state.setValue(value, address + offset)
          offset += TypeHelper.sizeof(theType)
        case info @ AddressInfo(_, theType) =>
          state.setValue(info.value.value, address + offset)
          offset += TypeHelper.sizeof(theType)
      }
  }
}

class ExecutionContext(parentVars: Map[String, Variable], val returnType: IType, val startingStackAddr: Int, state: State) {
  val visited = new ListBuffer[IASTNode]()
  val varMap = parentVars.clone()
  val pathStack = new Stack[IASTNode]()
  val stack = new Stack[Stackable]()

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

        val caseExpr = state.stack.pop.asInstanceOf[ValueInfo].value
        val switchExpr = state.stack.head
        
        val resolved = (switchExpr match {
          case info @ AddressInfo(_, _) => info.value
          case value @ ValueInfo(_, _) => value
        }).value

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
          case info @ AddressInfo(_,_) => info.value
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
            case info @ AddressInfo(addr, theType) => TypeHelper.cast(state.context.returnType, info.value.value)
            case value @ ValueInfo(_, _) => value
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

        state.stack.push(StringLiteral(decl.getName.getRawSignature))

        val name = state.stack.pop.asInstanceOf[StringLiteral].value

        decl match {
          case arrayDecl: IASTArrayDeclarator =>

            val dimensions = arrayDecl.getArrayModifiers.filter{_.getConstantExpression != null}.map{dim => state.stack.pop match {
              // can we can assume dimensions are integers
              case ValueInfo(value, _) => value.asInstanceOf[Int]
              case info @ AddressInfo(_, _) => info.value.value.asInstanceOf[Int]
            }}

            val initializer = decl.getInitializer.asInstanceOf[IASTEqualsInitializer]

            // Oddly enough, it is possible to have a pointer to an array with no dimensions OR initializer:
            //    extern char *x[]

            if (dimensions.isEmpty && initializer != null) {

              if (TypeHelper.resolve(theType).getKind == eChar && !initializer.getInitializerClause.isInstanceOf[IASTInitializerList]) {
                // e.g. char str[] = "Hello!\n";
                val initString = state.stack.pop.asInstanceOf[StringLiteral].value
                val strAddr = state.createStringVariable(initString, false)
                val theArrayPtr = new ArrayVariable(state, theType.asInstanceOf[IArrayType], Seq(initString.size))
                state.setValue(strAddr.value, theArrayPtr.address)
                state.context.addVariable(name, theArrayPtr)
              } else {
                val list = initializer.getInitializerClause.asInstanceOf[IASTInitializerList]
                val size = list.getSize

                val values: Array[ValueInfo] = (0 until size).map{x => state.stack.pop match {
                  case value @ ValueInfo(_,_) => value
                  case info @ AddressInfo(_,_) => info.value
                }}.reverse.toArray

                val theArrayPtr = new ArrayVariable(state, theType.asInstanceOf[IArrayType], Array(size))

                theArrayPtr.setArray(values)
                state.context.addVariable(name, theArrayPtr)
              }
            } else if (initializer != null) {
              val initVals: Array[Any] = (0 until initializer.getInitializerClause.getChildren.size).map{x => state.stack.pop}.reverse.toArray

              val theArrayVar: ArrayVariable = new ArrayVariable(state, theType.asInstanceOf[IArrayType], dimensions)

              val initialArray = initVals.map { newInit =>
                newInit match {
                  case StringLiteral(x) =>
                    state.createStringVariable(newInit.asInstanceOf[StringLiteral].value, false)
                  case info @ AddressInfo(_, _) => info.value
                  case value @ ValueInfo(_, _) => value
                }
              }

              state.setArray(initialArray, AddressInfo(theArrayVar.address + 4, theArrayVar.theType))
              state.context.addVariable(name, theArrayVar)
            } else {
              val theArrayVar = new ArrayVariable(state, theType.asInstanceOf[IArrayType], dimensions)
              state.context.addVariable(name, theArrayVar)
            }
          case decl: CASTDeclarator =>

            val stripped = TypeHelper.stripSyntheticTypeInfo(theType)

            val newVar = new Variable(state, theType)
            state.context.addVariable(name, newVar)

            if (!stripped.isInstanceOf[CStructure]) {
              val initVal = Option(decl.getInitializer).map(x => state.stack.pop).getOrElse(ValueInfo(0, null))
              BinaryExpr.parseAssign(op_assign, newVar, initVal)
            } else if (decl.getInitializer != null && decl.getInitializer.isInstanceOf[IASTEqualsInitializer]
                 && decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause.isInstanceOf[IASTInitializerList]) {

              val clause = decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause
              val values = clause.asInstanceOf[IASTInitializerList].getClauses.map{x => state.stack.pop}.reverse.toList
              newVar.setValues(values)
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
          
          var numArgs = 0

          val others = fcnDec.getChildren.filter{x => !x.isInstanceOf[IASTParameterDeclaration] && !x.isInstanceOf[IASTName]}
          
          if (!isInFunctionPrototype) {
            numArgs = state.stack.pop.asInstanceOf[ValueInfo].value.asInstanceOf[Integer]
            val args = (0 until numArgs).map{arg => state.stack.pop.asInstanceOf[ValueInfo]}.reverse
          
            args.foreach{ arg =>
              if (!isInFunctionPrototype && !paramDecls.isEmpty) {

                val paramDecl = paramDecls.pop
                
                val paramInfo = paramDecl.getDeclarator.getName.resolveBinding().asInstanceOf[CParameter]
  
                val name = paramDecl.getDeclarator.getName.getRawSignature
                val newVar = new Variable(state, paramInfo.getType)
                val casted = TypeHelper.cast(newVar.theType, arg.value).value
                state.setValue(casted, newVar.address)
            
                state.context.addVariable(name, newVar)
              } else {
                val theType = TypeHelper.getType(arg.value)
                val sizeof = TypeHelper.sizeof(theType)
                val space = state.allocateSpace(Math.max(sizeof, 4))
                state.setValue(arg.value, space)
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
              state.popFunctionContext
            }
            state.context.stack.push(retVal)
          } else {
            if (fcnDef.getDeclarator.getName.getRawSignature != "main") {
              state.popFunctionContext
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

          val result: TypeInfo = typeId.getDeclSpecifier match {
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
              }
              
              for (ptr <- typeId.getAbstractDeclarator.getPointerOperators) {
                result = new CPointerType(result, 0)
              }

              TypeInfo(result)
              
            case typespec: CASTTypedefNameSpecifier =>
              TypeInfo(typespec.getName.resolveBinding().asInstanceOf[IType])
            case elab: CASTElaboratedTypeSpecifier =>
              TypeInfo(elab.getName.resolveBinding().asInstanceOf[CStructure])
          }

          state.stack.push(result)
        }
        Seq()
    }
  }

  def init(codes: Seq[String], reset: Boolean, state: State) = {
    state.tUnit = Utils.getTranslationUnit(codes)
    state.current = state.tUnit

    if (reset) {
      state.functionContexts.push(new ExecutionContext(Map(), null, 0, state)) // load initial stack
    }

    val fcns = state.tUnit.getChildren.collect{case x:IASTFunctionDefinition => x}.filter(_.getDeclSpecifier.getStorageClass != IASTDeclSpecifier.sc_extern)
    val (mainFcn, others) = fcns.partition{fcnDef => fcnDef.getDeclarator.getName.getRawSignature == "main"}
    others.foreach{fcnDef => state.addFunctionDef(fcnDef)}
    
    run(state)

    state.context.pathStack.clear
    state.context.pathStack.push(mainFcn.head)
    state.current = state.context.pathStack.head
  }

  def tick(state: State): Unit = {
    state.direction = if (state.context.visited.contains(state.current)) Exiting else Entering

    //println(current.getClass.getSimpleName + ":" + direction)
    
    val paths: Seq[IASTNode] = Executor.step(state.current, state.direction)(state)
    
    if (state.isBreaking) {
      // unroll the path stack until we meet the first parent which is a loop
      
      val breakStatement = state.context.pathStack.pop.asInstanceOf[CASTBreakStatement]
      var reverse: IASTNode = breakStatement
      while ((!reverse.isInstanceOf[IASTWhileStatement] &&
          !reverse.isInstanceOf[IASTDoStatement] &&
          !reverse.isInstanceOf[IASTForStatement] &&
          !reverse.isInstanceOf[IASTSwitchStatement]) || !Utils.getAncestors(breakStatement).contains(reverse)) {
        reverse = state.context.pathStack.pop
      }

      state.isBreaking = false
    }


    if (state.isContinuing) {
      // unroll the path stack until we meet the first parent which is a loop

      val continueStatement = state.context.pathStack.pop.asInstanceOf[CASTContinueStatement]
      var last: IASTNode = continueStatement

      // find the first for loop that is a direct ancestor
      while (!last.isInstanceOf[IASTForStatement] || !Utils.getAncestors(continueStatement).contains(last)) {
        last = state.context.pathStack.pop
      }

      val forLoop = last.asInstanceOf[IASTForStatement]

      state.context.pathStack.push(forLoop)
      state.context.pathStack.push(forLoop.getConditionExpression)
      state.context.pathStack.push(forLoop.getIterationExpression)

      state.isContinuing = false
    }
    
    if (state.isReturning) {
      var last: IASTNode = null
      while (state.context.pathStack.size > 1 && !last.isInstanceOf[IASTFunctionDefinition]) {
        last = state.context.pathStack.pop
      }

      state.current = state.context.pathStack.head
      state.isReturning = false
    } else {

      if (state.direction == Exiting) {
        state.context.pathStack.pop
      } else {
        state.context.visited += state.current
      }
  
      paths.reverse.foreach { path => state.context.pathStack.push(path) }
  
      if (!state.context.pathStack.isEmpty) {
        state.current = state.context.pathStack.head
      } else {
        state.current = null
      }
    }
  }

  def run(state: State) = {
    while (state.current != null) {
      tick(state)
    }
  }
}
