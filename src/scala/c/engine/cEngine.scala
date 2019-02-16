package scala.c.engine

import org.eclipse.cdt.core.dom.ast.{IASTCaseStatement, IASTDeclarationStatement, IASTEqualsInitializer, _}

import scala.collection.mutable.ListBuffer
import java.nio.ByteBuffer
import java.nio.ByteOrder

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.op_assign
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.c.engine.ast.BinaryExpr.evaluate
import scala.c.engine.ast.Expressions.evaluate
import scala.c.engine.ast.{Ast, BinaryExpr, Declarator, Expressions}


object Interpreter {
  implicit class CounterSC(val sc: StringContext) extends AnyVal {

    // Define functions that we want to use with string interpolation syntax
    def c(args: Any*)(implicit state: State): Unit = {
      Gcc.runCode(sc.parts.iterator.next, state)
    }

    def func(args: Any*)(implicit state: State): Unit = {
      Gcc.runGlobalCode(sc.parts.iterator.next, state)
    }
  }
}

class Memory(size: Int) {

  import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._

  var insertIndex = 0
  // turing tape
  val tape = ByteBuffer.allocate(size)
  tape.order(ByteOrder.LITTLE_ENDIAN)

  def clearMemory(startingAddress: Int, numBytes: Int) = {
    var address = startingAddress
    for (i <- 0 until numBytes) {
      tape.put(address, 0.toByte)
      address += 1
    }
  }

  // use Address type to prevent messing up argument order
  def writeToMemory(newVal: AnyVal, address: Int, theType: IType, bitOffset: Int = 0, sizeInBits: Int = 0): Unit = {

    TypeHelper.stripSyntheticTypeInfo(theType) match {
      case basic: IBasicType if basic.getKind == eInt && basic.isShort =>
          newVal match {
            case int: Int => tape.putShort(address, int.asInstanceOf[Short])
            case short: Short => tape.putShort(address, short)
          }
      case basic: IBasicType if basic.getKind == eInt && basic.isLongLong =>
        newVal match {
          case long: Long => tape.putLong(address, long)
        }
      case basic: IBasicType if basic.getKind == eInt && basic.isLong =>
        newVal match {
          case int: Int => tape.putInt(address, int)
          case long: Long => tape.putInt(address, long.toInt)
        }
      case basic: IBasicType if basic.getKind == eInt || basic.getKind == eVoid =>
        newVal match {
          case int: Int =>
            val x = if (bitOffset != 0) {
              val currentVal = tape.getInt(address)
              val right = currentVal << (32 - bitOffset) >>> (32 - bitOffset)
              val left = currentVal >>> (sizeInBits + bitOffset) << (sizeInBits + bitOffset)

              val newVal = int << bitOffset
              left + newVal + right
            } else {
              int
            }

            tape.putInt(address, x)
          case long: Long => tape.putInt(address, long.toInt)
        }
      case basic: IBasicType if basic.getKind == eDouble =>
        newVal match {
          case int: Int => tape.putDouble(address, int.toDouble)
          case double: Double => tape.putDouble(address, double)
        }
      case basic: IBasicType if basic.getKind == eFloat =>
        newVal match {
          case float: Float => tape.putFloat(address, float)
        }
      case basic: IBasicType if basic.getKind == eChar =>
        newVal match {
          case char: char => tape.put(address, char)
          case int: Int => tape.put(address, int.toByte)
        }
      case basic: IBasicType if basic.getKind == eBoolean =>
        tape.putInt(address, newVal.asInstanceOf[Int])
      case _: IFunctionType =>
        writePointerToMemory(newVal, address)
      case _: CStructure =>
        writePointerToMemory(newVal, address)
      case _: IPointerType =>
        writePointerToMemory(newVal, address)
      case _: IArrayType =>
        writePointerToMemory(newVal, address)
    }
  }

  private def writePointerToMemory(newVal: AnyVal, address: Int) = {
    newVal match {
      case int: Int => tape.putInt(address, int)
      case long: Long => tape.putInt(address, long.toInt)
    }
  }

  def readFromMemory(address: Int, theType: IType, bitOffset: Int = 0, sizeInBits: Int = 0): RValue = {
    val result: AnyVal = theType match {
      case basic: IBasicType =>
        if (basic.getKind == eInt && basic.isShort) {
          var result = tape.getShort(address)
          if (sizeInBits != 0) {
            result = (result << (16 - sizeInBits - bitOffset) >>> (16 - sizeInBits)).toShort
          }
          result
        } else if (basic.getKind == eInt && basic.isLongLong) {
          var result = tape.getLong(address)
          if (sizeInBits != 0) {
            result = result << (64 - sizeInBits - bitOffset) >>> (64 - sizeInBits)
          }
          result
        } else if (basic.getKind == eInt && basic.isLong) {
          var result = tape.getInt(address)
          if (sizeInBits != 0) {
            result = result << (32 - sizeInBits - bitOffset) >>> (32 - sizeInBits)
          }
          result
        } else if (basic.getKind == eInt) {
          var result = tape.getInt(address)
          if (sizeInBits != 0) {
            result = result << (32 - sizeInBits - bitOffset) >>> (32 - sizeInBits)
          }
          result
        } else if (basic.getKind == eBoolean) {
          var result = tape.getInt(address)
          if (sizeInBits != 0) {
            result = result << (32 - sizeInBits - bitOffset) >> (32 - sizeInBits)
          }
          result
        } else if (basic.getKind == eDouble) {
          tape.getDouble(address)
        } else if (basic.getKind == eFloat) {
          tape.getFloat(address)
        } else if (basic.getKind == eChar) {
          tape.get(address) // a C 'char' is a Java 'byte'
        } else if (basic.getKind == eVoid) {
          tape.getInt(address)
        } else {
          throw new Exception("Bad read val: " + basic.getKind)
        }
      case ptr: IPointerType => tape.getInt(address)
      case fcn: IFunctionType => tape.getInt(address)
      case struct: CStructure => tape.getInt(address)
      case qual: IQualifierType => readFromMemory(address, qual.getType).value
      case typedef: CTypedef => readFromMemory(address, typedef.getType).value
    }


    TypeHelper.castSign(theType, result)
  }
}

case class ReturnFromFunction() extends Exception("returning")

case class JmpIfNotEqual(expr: IASTExpression, relativeJump: Int)
case class JmpToLabelIfNotZero(expr: IASTExpression, label: Label)
case class JmpToLabelIfZero(expr: IASTExpression, label: Label)
case class JmpToLabelIfEqual(expr1: IASTExpression, expr2: IASTExpression, label: Label)
case class Jmp(relativeJump: Int)
case class JmpLabel(label: Label)
case class JmpName(label: String) {
  var destAddress = 0
}

abstract class Label {
  var address = 0
}

case class PushVariableStack()
case class PopVariableStack()

case class GotoLabel(name: String) extends Label
case class BreakLabel() extends Label
case class ContinueLabel() extends Label
case class GenericLabel() extends Label

case class CaseLabel(caseStatement: IASTCaseStatement) extends Label
case class DefaultLabel(default: IASTDefaultStatement) extends Label

object State {
  def flattenNode(tUnit: IASTNode)(implicit state: State): List[Any] = {

    def recurse(node: IASTNode): List[Any] = {

      node match {
        case null => List()

        case ifStatement: IASTIfStatement =>
          val contents = PushVariableStack() +: recurse(ifStatement.getThenClause) :+ PopVariableStack()
          val elseContents = PushVariableStack() +: List(Option(ifStatement.getElseClause)).flatten.flatMap(recurse) :+ PopVariableStack()

          val jmp = if (ifStatement.getElseClause != null) {
            List(Jmp(elseContents.size))
          } else {
            List()
          }

          // add +1 for the jmp statement
          JmpIfNotEqual(ifStatement.getConditionExpression, (contents ++ jmp).size) +: ((contents ++ jmp) ++ elseContents)
        case forStatement: IASTForStatement =>

          val breakLabel = BreakLabel()
          state.breakLabelStack = breakLabel +: state.breakLabelStack
          val continueLabel = ContinueLabel()
          state.continueLabelStack = continueLabel +: state.continueLabelStack

          val init = List(forStatement.getInitializerStatement)
          val contents = recurse(forStatement.getBody)
          val iter = forStatement.getIterationExpression
          val beginLabel = new GotoLabel("")

          state.breakLabelStack = state.breakLabelStack.tail
          state.continueLabelStack = state.continueLabelStack.tail

          val execution = contents ++ List(continueLabel, iter)

          val result = (if (forStatement.getConditionExpression != null) {
            init ++ (beginLabel +: JmpToLabelIfNotZero(forStatement.getConditionExpression, breakLabel) +: execution :+ JmpLabel(beginLabel)) :+ breakLabel
          } else {
            init ++ (beginLabel +: execution :+ JmpLabel(beginLabel)) :+ breakLabel
          })

          PushVariableStack() +: result :+ PopVariableStack()
        case whileStatement: IASTWhileStatement =>

          val breakLabel = BreakLabel()
          state.breakLabelStack = breakLabel +: state.breakLabelStack
          val continueLabel = ContinueLabel()
          state.continueLabelStack = continueLabel +: state.continueLabelStack

          val contents = recurse(whileStatement.getBody)
          val begin = new GotoLabel("")
          val end = new GotoLabel("")

          state.breakLabelStack = state.breakLabelStack.tail
          state.continueLabelStack = state.continueLabelStack.tail

          val result = List(JmpLabel(end), begin) ++ contents ++ List(end, continueLabel, JmpToLabelIfZero(whileStatement.getCondition, begin)) :+ breakLabel

          PushVariableStack() +: result :+ PopVariableStack()
        case doWhileStatement: IASTDoStatement =>

          val breakLabel = BreakLabel()
          state.breakLabelStack = breakLabel +: state.breakLabelStack
          val continueLabel = ContinueLabel()
          state.continueLabelStack = continueLabel +: state.continueLabelStack

          val contents = recurse(doWhileStatement.getBody)
          val begin = new GenericLabel()

          state.breakLabelStack = state.breakLabelStack.tail
          state.continueLabelStack = state.continueLabelStack.tail

          val result = List(begin) ++ contents ++ List(continueLabel, JmpToLabelIfZero(doWhileStatement.getCondition, begin)) :+ breakLabel

          PushVariableStack() +: result :+ PopVariableStack()
        case switch: IASTSwitchStatement =>

          val breakLabel = BreakLabel()
          state.breakLabelStack = breakLabel +: state.breakLabelStack

          val descendants = recurse(switch.getBody)

          def getParentSwitchBody(node: IASTNode): IASTStatement = {
            if (node.getParent.isInstanceOf[IASTSwitchStatement]) {
              node.getParent.asInstanceOf[IASTSwitchStatement].getBody
            } else {
              getParentSwitchBody(node.getParent)
            }
          }

          val jumpTable = descendants.flatMap{
            case x @ CaseLabel(caseStatement) if (switch.getBody == getParentSwitchBody(caseStatement)) =>
              List(JmpToLabelIfEqual(caseStatement.getExpression, switch.getControllerExpression, x))
            case x @ DefaultLabel(default) if (switch.getBody == getParentSwitchBody(default)) =>
              List(JmpLabel(x))
            case _ =>
              List()
          } :+ JmpLabel(breakLabel)

          state.breakLabelStack = state.breakLabelStack.tail

          val result = jumpTable ++ descendants :+ breakLabel

          PushVariableStack() +: result :+ PopVariableStack()
        case x: IASTCaseStatement =>
          List(CaseLabel(x))
        case x: IASTDefaultStatement =>
          List(DefaultLabel(x))
        case _: IASTContinueStatement =>
          List(JmpLabel(state.continueLabelStack.head))
        case _: IASTBreakStatement =>
          List(JmpLabel(state.breakLabelStack.head))
        case _: IASTCompositeTypeSpecifier =>
          List()
        case _: IASTElaboratedTypeSpecifier =>
          List()
        case goto: IASTGotoStatement =>
          List(JmpName(goto.getName.getRawSignature))
        case fcn: IASTFunctionDefinition =>
          List(fcn)
        case compound: IASTCompoundStatement =>

          val isTypicalCompound = compound.getParent() match {
            case x: IASTSwitchStatement => true
            case x: CASTFunctionDefinition => true
            case x: CASTForStatement => true
            case x: CASTIfStatement => true
            case x: CASTDoStatement => true
            case x: CASTWhileStatement => true
            case _ => false
          }

          if (isTypicalCompound) {
            compound.getStatements.flatMap(recurse).toList
          } else {
            PushVariableStack() +: compound.getStatements.flatMap(recurse).toList :+ PopVariableStack()
          }
        case decl: IASTDeclarationStatement =>
          decl.getChildren.toList.flatMap(recurse)
        case decl: CASTSimpleDeclaration =>
          List(decl)
        case _: IASTSimpleDeclSpecifier =>
          List()
        case _: CASTTypedefNameSpecifier =>
          List()
        case decl: IASTDeclarator =>
          List(decl)
        case eq: IASTEqualsInitializer =>
          recurse(eq.getInitializerClause)
        case label: IASTLabelStatement =>
          GotoLabel(label.getName.getRawSignature) +: recurse(label.getNestedStatement)
        case exprState: CASTExpressionStatement =>
          List(exprState.getExpression)
        case fcn: IASTFunctionCallExpression =>
          List(fcn)
        case enum: IASTEnumerationSpecifier =>
          List(enum)
        case _ =>
          //println("SPLITTING: " + node.getClass.getSimpleName + " : " + node.getRawSignature)
          node +: node.getChildren.toList
      }
    }

    tUnit.getChildren.flatMap{recurse}.toList
  }
}

abstract sealed trait NumBits
case object ThirtyTwoBits extends NumBits
case object SixtyFourBits extends NumBits

class State(pointerSize: NumBits) {

  object Stack extends Memory(500000)

  var heapInsertIndex = 250000

  val functionPrototypes = scala.collection.mutable.LinkedHashSet[IASTFunctionDeclarator]()

  var functionContexts = List[FunctionScope]()
  def context: FunctionScope = functionContexts.head
  val functionList = new ListBuffer[Function]()
  val functionPointers = scala.collection.mutable.LinkedHashMap[String, Variable]()
  val stdout = new ListBuffer[Char]()
  var functionCount = 0
  var mainFunc: IASTFunctionDefinition = null

  private var breakLabelStack = List[Label]()
  private var continueLabelStack = List[Label]()

  val declarations = new ListBuffer[CStructure]()

  def numScopes = functionContexts.size

  val pointerType = pointerSize match {
    case ThirtyTwoBits => new CBasicType(IBasicType.Kind.eInt, 0)
    case SixtyFourBits => new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG_LONG)
  }

  def pushScope(scope: FunctionScope): Unit = {
    functionContexts = scope +: functionContexts
  }

  def getFunctionScope = {
    functionContexts.collect{case fcnScope: FunctionScope => fcnScope}.head
  }

  def popFunctionContext = {
    Stack.insertIndex = functionContexts.head.startingStackAddr
    functionContexts = functionContexts.tail
  }

  def hasFunction(name: String): Boolean = functionList.exists{fcn => fcn.name == name}
  def getFunction(name: String): Function = functionList.find{fcn => fcn.name == name}.get
  def getFunctionByIndex(index: Int): Function = functionList.find{fcn => fcn.index == index}.get

  Functions.scalaFunctions.foreach{fcn =>
    addScalaFunctionDef(fcn)
  }

  val program = new FunctionScope(List(), null, null) {}
  pushScope(program)
  addMain()

  def init(codes: Seq[String]): IASTNode = {
    val tUnit = Utils.getTranslationUnit(codes)

    val fcns = tUnit.getChildren.collect{case x:IASTFunctionDefinition => x}
                                .filter(fcn => fcn.getDeclSpecifier.getStorageClass != IASTDeclSpecifier.sc_extern && fcn.getDeclarator.getName.getRawSignature != "main")
    fcns.foreach{fcnDef => addFunctionDef(fcnDef)}

    tUnit.getChildren.collect{case x:IASTFunctionDefinition => x}
      .find(fcn => fcn.getDeclarator.getName.getRawSignature == "main").foreach { fcn =>
      mainFunc = fcn
    }

    functionContexts = List[FunctionScope]()

    declarations ++= tUnit.getDeclarations.collect{case simp: CASTSimpleDeclaration => simp.getDeclSpecifier}
      .collect{case comp: CASTCompositeTypeSpecifier => comp}
      .map{x => x.getName.resolveBinding().asInstanceOf[CStructure]}

    tUnit
  }

  def addScalaFunctionDef(fcn: Function) = {

    fcn.index = functionCount

    functionList += fcn

    val fcnType = new CFunctionType(new CBasicType(IBasicType.Kind.eVoid, 0), null)
    val newVar = Variable(fcn.name, State.this, fcnType)
    Stack.writeToMemory(functionCount, newVar.address, fcnType)

    functionPointers += fcn.name -> newVar
    functionCount += 1
  }

  private def addStaticFunctionVars(node: IASTNode)(implicit state: State): List[Variable] = {
    node match {
      case decl: IASTDeclarator =>
        val nameBinding = decl.getName.resolveBinding()

        nameBinding match {
          case vari: IVariable =>
            if (vari.isStatic) {
              val theType = TypeHelper.stripSyntheticTypeInfo(nameBinding.asInstanceOf[IVariable].getType)

              val variable = Variable(decl.getName.getRawSignature, state, vari.getType)
              if (decl.getInitializer != null) {
                val initVals = Declarator.getRValues(decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause, theType)
                Declarator.assign(variable, initVals, null, op_assign)
              }

              variable.isInitialized = true

              List(variable)
            } else {
              List()
            }
          case _ => List()
        }
      case x => x.getChildren.toList.flatMap{x => addStaticFunctionVars(x)}
    }
  }

  def addFunctionDef(fcnDef: IASTFunctionDefinition) = {
    val name = fcnDef.getDeclarator.getName

    val fcnType = fcnDef.getDeclarator.getName.resolveBinding().asInstanceOf[IFunction].getType

    functionList += new Function(name.getRawSignature, true) {
      index = functionCount
      node = fcnDef
      override val staticVars = addStaticFunctionVars(fcnDef)(State.this)
      def parameters = fcnType.getParameterTypes.toList
      def run(formattedOutputParams: Array[RValue], state: State): Option[AnyVal] = {
        None
      }
    }

    val newVar = Variable(name.getRawSignature, State.this, fcnType)
    Stack.writeToMemory(functionCount, newVar.address, fcnType)

    functionPointers += name.getRawSignature -> newVar
    functionCount += 1
  }

  def addMain() = {
    val name = "main"

    functionList += new Function(name, true) {
      index = 0
      node = null
      override val staticVars = List()
      def parameters = List()
      def run(formattedOutputParams: Array[RValue], state: State): Option[AnyVal] = {None}
    }

    val mainType = new CFunctionType(new CBasicType(IBasicType.Kind.eVoid, 0), Array())

    val newVar = Variable(name, State.this, mainType)
    Stack.writeToMemory(functionCount, newVar.address, mainType)

    functionPointers += name -> newVar
    functionCount += 1
  }

  def callFunctionFromScala(name: String, args: Array[RValue]): Seq[IASTNode] = {

    functionList.find(_.name == name).map { fcn =>
      // this is a function simulated in scala
      fcn.run(args.reverse, this).foreach { retVal => context.pushOntoStack(List(new RValue(retVal, null) {})) }
    }

    Seq()
  }

  def callTheFunction(name: String, call: IASTFunctionCallExpression, scope: Option[FunctionScope], isApi: Boolean = false)(implicit state: State): Option[ValueType] = {

    functionList.find(_.name == name).map{ function =>

      if (!function.isNative) {
        // this is a function simulated in scala

        val stackPos = Stack.insertIndex
        val args = call.getArguments.map{x => Expressions.evaluate(x)}
        val resolvedArgs: Array[RValue] = args.flatten.map{ TypeHelper.resolve }
        val returnVal = function.run(resolvedArgs.reverse, this)
        Stack.insertIndex = stackPos // pop the stack
        returnVal.map{theVal => new RValue(theVal, null) {}}
      } else {

        val newScope = scope.getOrElse {
          val expressionType = if (call != null) {
            call.getExpressionType
          } else {
            null
          }

          new FunctionScope(function.staticVars, functionContexts.headOption.getOrElse(null), expressionType)
        }

        if (function.name == "main") {
          if (isApi) {
            scope.get.init(state.mainFunc, this, !scope.isDefined)
            functionContexts = List(scope.get)
            context.run(this)
            None
          } else {
            newScope.init(state.mainFunc, this, !scope.isDefined)
            newScope.pushOntoStack(List(new RValue(0, null) {}))
            functionContexts = newScope +: functionContexts
            context.run(this)
            popFunctionContext
            None
          }
        } else {
          newScope.init(function.node, this, !scope.isDefined)

          val args: List[ValueType] = if (call != null) {
            call.getArguments.map { x => Expressions.evaluate(x).head }.toList
          } else {
            List()
          }
          val resolvedArgs = args.map{ TypeHelper.resolve }

          // printf assumes all floating point numbers are doubles
          // and shorts are 4 bytes
          val promoted = resolvedArgs.map{arg =>
            if (arg.theType.isInstanceOf[IBasicType] && arg.theType.asInstanceOf[IBasicType].getKind == IBasicType.Kind.eFloat) {
              TypeHelper.cast(new CBasicType(IBasicType.Kind.eDouble, 0), arg.value)
            } else if (arg.theType.isInstanceOf[IBasicType] && arg.theType.asInstanceOf[IBasicType].isShort) {
              TypeHelper.cast(new CBasicType(IBasicType.Kind.eInt, 0), arg.value)
            } else if (arg.theType.isInstanceOf[IBasicType] && arg.theType.asInstanceOf[IBasicType].getKind == IBasicType.Kind.eChar) {
              TypeHelper.cast(new CBasicType(IBasicType.Kind.eInt, 0), arg.value)
            } else {
              arg
            }
          }

          newScope.pushOntoStack(promoted :+ new RValue(resolvedArgs.size, null) {})

          functionContexts = newScope +: functionContexts

          context.run(this)

          val returnVal = context.getReturnValue

          popFunctionContext
          returnVal
        }
      }
    }.getOrElse{
      // function pointer case
      val fcnPointer = functionContexts.head.resolveId(new CASTName(name.toCharArray)).get
      val function = getFunctionByIndex(fcnPointer.value.asInstanceOf[Int])
      val scope = new FunctionScope(function.staticVars, functionContexts.head, call.getExpressionType)
      functionContexts = functionContexts :+ scope
      scope.init(call, this, false)
      //context.pathStack.push(NodePath(function.node, Stage1))
      context.run(this)
      popFunctionContext
      None
    }
  }

  def allocateSpace(numBytes: Int): Int = {
    if (numBytes > 0) {
      val result = Stack.insertIndex
      Stack.insertIndex += numBytes
      result
    } else {
      0
    }
  }

  def allocateHeapSpace(numBytes: Int): Int = {
    if (numBytes > 0) {
      val result = heapInsertIndex
      heapInsertIndex += numBytes
      result
    } else {
      0
    }
  }

  def copy(dst: Int, src: Int, numBytes: Int) = {
    if (numBytes != 0) {
      for (i <- (0 until numBytes)) {
         Stack.tape.put(dst + i, Stack.tape.get(src + i))
      }
    }
  }

  def readPtrVal(address: Int): Int = {
    Stack.readFromMemory(address, pointerType).value match {
      case int: Int => int
      case double: Double => double.toInt
    }
  }

  def createStringVariable(str: String, isHeap: Boolean)(implicit state: State): Address = {
    val theStr = Utils.stripQuotes(str)

    val withNull = (theStr.toCharArray() :+ 0.toChar).map{char => new RValue(char.toByte, new CBasicType(IBasicType.Kind.eChar, 0)) {}} // terminating null char
    val strAddr = if (isHeap) allocateHeapSpace(withNull.size) else allocateSpace(withNull.size)

    writeDataBlock(withNull, strAddr)
    new Address(strAddr, pointerType) {}
  }

  def writeDataBlock(array: Array[RValue], startingAddress: Int)(implicit state: State): Unit = {
      var address = startingAddress
      array.foreach { element =>
        element match {
          case RValue(newVal, theType) =>
            Stack.writeToMemory(newVal, address, theType)
            address +=  TypeHelper.sizeof(theType)(state)
        }
      }
  }
}