package c.engine

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import java.nio.ByteBuffer
import java.nio.ByteOrder

import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.c.engine.{Scope, FunctionScope, NodePath}

object Interpreter {
  implicit val state = new State
  
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

  // use Address type to prevent messing up argument order
  def writeToMemory(newVal: AnyVal, address: Int, theType: IType): Unit = {

    TypeHelper.stripSyntheticTypeInfo(theType) match {
      case basic: IBasicType if basic.getKind == eInt && basic.isShort =>
          newVal match {
            case int: Int => tape.putShort(address, int.asInstanceOf[Short])
            case short: Short => tape.putShort(address, short)
          }
      case basic: IBasicType if basic.getKind == eInt && basic.isLong =>
        newVal match {
          case long: Long => tape.putInt(address, long.toInt)
        }
      case basic: IBasicType if basic.getKind == eInt || basic.getKind == eVoid =>
        newVal match {
          case int: Int => tape.putInt(address, int)
        }
      case basic: IBasicType if basic.getKind == eDouble =>
        newVal match {
          case double: Double => tape.putDouble(address, double)
        }
      case basic: IBasicType if basic.getKind == eFloat =>
        newVal match {
          case float: Float => tape.putFloat(address, float)
        }
      case basic: IBasicType if basic.getKind == eChar =>
        newVal match {
          case char: char => tape.put(address, char)
          case int: Int => tape.putInt(address, int)
        }
      case basic: IBasicType if basic.getKind == eBoolean =>
        newVal match {
          case char: char => tape.put(address, char)
          case int: Int => tape.putInt(address, int)
        }
      case basic: IFunctionType =>
        newVal match {
          case int: Int => tape.putInt(address, int)
        }
      case basic: CStructure =>
        newVal match {
          case int: Int => tape.putInt(address, int)
        }
      case ptr: IPointerType =>
        newVal match {
          case int: Int => tape.putInt(address, int)
          case long: Long => tape.putInt(address, long.toInt)
        }
    }
  }

  def readFromMemory(address: Int, theType: IType): RValue = {
    val result: AnyVal = theType match {
      case basic: IBasicType =>
        if (basic.getKind == eInt && basic.isShort) {
          tape.getShort(address)
        }  else if (basic.getKind == eInt && basic.isLong) {
          tape.getInt(address)
        } else if (basic.getKind == eInt) {
          tape.getInt(address)
        } else if (basic.getKind == eBoolean) {
          tape.getInt(address)
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

class State {

  var tUnit: IASTTranslationUnit = null

  object Stack extends Memory(100000)

  var heapInsertIndex = 50000

  val functionPrototypes = scala.collection.mutable.HashSet[IASTFunctionDeclarator]()

  private val functionContexts = new Stack[Scope]()
  def context = functionContexts.head
  val functionList = new ListBuffer[Function]()
  val functionPointers = scala.collection.mutable.Map[String, Variable]()
  val stdout = new ListBuffer[Char]()
  var functionCount = 0
  def stack = context.stack

  // flags
  var isGotoing = false
  var gotoName = ""

  var nextGotoNode: Seq[IASTNode] = Seq()

  functionContexts.push(new FunctionScope(List(), null, null, this))

  private val scopeCache = new scala.collection.mutable.HashMap[IASTNode, Scope]()

  def numScopes = functionContexts.size

  def pushScope(scope: Scope, node: IASTNode): Unit = {

//    if (scopeCache.contains(node)) {
//      val scope = scopeCache(node)
//      scope.reset
//      functionContexts.push(scope)
//    } else {
//      scopeCache += node -> scope
      functionContexts.push(scope)
 //   }
  }

  def getFunctionScope = {
    functionContexts.collect{case fcnScope: FunctionScope => fcnScope}.head
  }

  def popFunctionContext = {
    Stack.insertIndex = functionContexts.head.startingStackAddr
    functionContexts.pop
  }

  def hasFunction(name: String): Boolean = functionList.exists{fcn => fcn.name == name}
  def getFunction(name: String): Function = functionList.find{fcn => fcn.name == name}.get
  def getFunctionByIndex(index: Int): Function = functionList.find{fcn => fcn.index == index}.get

  Functions.scalaFunctions.foreach{fcn =>
    addScalaFunctionDef(fcn)
  }

  def addScalaFunctionDef(fcn: Function) = {

    fcn.index = functionCount

    functionList += fcn

    val fcnType = new CFunctionType(new CBasicType(IBasicType.Kind.eVoid, 0), null)

    val newVar = new Variable(new CASTName(fcn.name.toCharArray), State.this, fcnType)
    Stack.writeToMemory(functionCount, newVar.address, fcnType)

    functionPointers += fcn.name -> newVar
    functionCount += 1
  }

  private def addStaticFunctionVars(node: IASTNode, state: State): List[Variable] = {
    node match {
      case decl: IASTDeclarator =>
        val nameBinding = decl.getName.resolveBinding()

        if (nameBinding.isInstanceOf[IVariable]) {
          val theType = TypeHelper.stripSyntheticTypeInfo(nameBinding.asInstanceOf[IVariable].getType)

          if (decl.getParent.isInstanceOf[IASTSimpleDeclaration] &&
            decl.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclSpecifier.getStorageClass == IASTDeclSpecifier.sc_static) {
            List(new Variable(decl.getName, state, theType))
          } else {
            List()
          }
        } else {
          List()
        }
      case x => x.getChildren.toList.flatMap{x => addStaticFunctionVars(x, state)}
    }
  }

  def addFunctionDef(fcnDef: IASTFunctionDefinition) = {
    val name = fcnDef.getDeclarator.getName

    val fcnType = fcnDef.getDeclarator.getName.resolveBinding().asInstanceOf[IFunction].getType


//    def findLabels(node: IASTNode): List[IASTLabelStatement] = {
//
//      @tailrec
//      def recurse(nodes: List[IASTNode], acc: List[IASTLabelStatement]): List[IASTLabelStatement] = {
//        nodes match {
//          case Nil => acc
//          case x :: tail =>
//            val label: Seq[IASTLabelStatement] = if (x.isInstanceOf[IASTLabelStatement]) {
//              Seq(x.asInstanceOf[IASTLabelStatement])
//            } else {
//              Seq()
//            }
//            recurse(tail, acc ++ label)
//        }
//      }
//
//      recurse(List(node), List())
//    }

    functionList += new Function(name.getRawSignature, true) {
      index = functionCount

      override val staticVars = addStaticFunctionVars(fcnDef, State.this)
      def parameters = fcnType.getParameterTypes.toList
      def run(formattedOutputParams: Array[RValue], state: State): Option[AnyVal] = {None}
      override def node = fcnDef
    }

    val newVar = new Variable(name, State.this, fcnType)
    Stack.writeToMemory(functionCount, newVar.address, fcnType)

    functionPointers += name.getRawSignature -> newVar
    functionCount += 1
  }

  def callFunctionFromScala(name: String, args: Array[RValue]): Seq[IASTNode] = {

    functionList.find(_.name == name).map { fcn =>
      // this is a function simulated in scala
      fcn.run(args.reverse, this).foreach { retVal => context.stack.push(RValue(retVal, null)) }
    }

    Seq()
  }

  def callTheFunction(name: String, call: IASTFunctionCallExpression, args: Array[ValueType]): Seq[IASTNode] = {

    functionList.find(_.name == name).map{ function =>

      functionContexts.push(new FunctionScope(function.staticVars, functionContexts.head, new CFunctionType(call.getExpressionType, null), this))

      val resolvedArgs: Array[RValue] = args.map{x =>
        x match {
          case StringLiteral(str) =>
            createStringVariable(str, false)(this)
          case info @ LValue(_, _) => info.value
          case value @ RValue(_, _) => value
        }
      }

      val convertedArgs = functionPrototypes.find{_.getName.getRawSignature == name}.map{ proto =>
        val params = proto.getChildren.collect{case param: CASTParameterDeclaration => param}
        var i = -1
        params.map{ p =>
          i += 1
          if (p.getDeclSpecifier.isInstanceOf[CASTSimpleDeclSpecifier]) {
            val param = p.getDeclarator.getName.resolveBinding().asInstanceOf[IParameter]
            TypeHelper.cast(param.getType, resolvedArgs(i).value)
          } else {
            resolvedArgs(i)
          }
        } ++ resolvedArgs.drop(i + 1)
      }.getOrElse {
        resolvedArgs
      }

      if (!function.isNative) {
        // this is a function simulated in scala
        val returnVal = function.run(resolvedArgs.reverse, this)

        popFunctionContext

        returnVal.foreach{ retVal =>
          context.stack.push(RValue(retVal, null))
        }

        Seq()
      } else {
        context.pathStack.push(NodePath(call, Stage1))
        convertedArgs.foreach{ arg => context.stack.push(arg)}
        context.stack.push(RValue(resolvedArgs.size, null))
        Seq(function.node)
      }
    }.getOrElse{
      // function pointer case
      val fcnPointer = functionContexts.head.resolveId(new CASTName(name.toCharArray)).get
      val fcn = getFunctionByIndex(fcnPointer.value.asInstanceOf[Int])
      Seq(fcn.node)
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
    Stack.readFromMemory(address, TypeHelper.pointerType).value.asInstanceOf[Int]
  }

  def createStringVariable(str: String, isHeap: Boolean)(implicit state: State): RValue = {
    val theStr = Utils.stripQuotes(str)
    val translateLineFeed = theStr.replace("\\n", 10.asInstanceOf[Char].toString)
    val withNull = (translateLineFeed.toCharArray() :+ 0.toChar).map{char => RValue(char.toByte, new CBasicType(IBasicType.Kind.eChar, 0))} // terminating null char
    val strAddr = if (isHeap) allocateHeapSpace(withNull.size) else allocateSpace(withNull.size)

    setArray(withNull, strAddr, 1)
    RValue(strAddr, TypeHelper.pointerType)
  }

  def setArray(array: Array[RValue], address: Int, stride: Int): Unit = {
      var i = 0
      array.foreach { element =>
        element match {
          case RValue(newVal, theType) =>
            Stack.writeToMemory(newVal, address + i, theType)
        }

        i += stride
      }
  }
}