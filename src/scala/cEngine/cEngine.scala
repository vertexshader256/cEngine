package cEngine

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import java.nio.ByteBuffer
import java.nio.ByteOrder

import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.annotation.tailrec

object Interpreter {
  type JSONObject = Any
  
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


class State {

  var tUnit: IASTTranslationUnit = null
  var current: IASTNode = null
  var direction: Direction = Entering

  // turing tape 
  private val tape = ByteBuffer.allocate(100000);
  tape.order(ByteOrder.LITTLE_ENDIAN)

  var stackInsertIndex = 0
  var heapInsertIndex = 50000

  var standardOutBuffer = new ListBuffer[Char]
  val functionContexts = new Stack[ExecutionContext]()
  def context = functionContexts.head
  val functionList = new ListBuffer[Function]()
  val functionPointers = scala.collection.mutable.Map[String, Variable]()
  val stdout = new ListBuffer[String]()
  var functionCount = 0
  def stack = context.stack

  var isFirst = true
  // flags
  var isReturning = false
  var isBreaking = false
  var isContinuing = false
  var isGotoing = false

  functionContexts.push(new ExecutionContext(List(), List(), null, 0, this))

  def popFunctionContext = {
    stackInsertIndex = functionContexts.head.startingStackAddr
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
    
    val newVar = new Variable(fcn.name, State.this, fcnType)
    setValue(functionCount, newVar.address)
    
    functionPointers += fcn.name -> newVar
    functionCount += 1
  }

  private def addStaticFunctionVars(node: IASTNode, state: State): List[Variable] = {
    node match {
      case decl: IASTDeclarator =>
        val nameBinding = decl.getName.resolveBinding()

        if (nameBinding.isInstanceOf[IVariable]) {
          val theType = TypeHelper.stripSyntheticTypeInfo(nameBinding.asInstanceOf[IVariable].getType)

          val name = decl.getName.getRawSignature
          if (decl.getParent.isInstanceOf[IASTSimpleDeclaration] &&
            decl.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclSpecifier.getStorageClass == IASTDeclSpecifier.sc_static) {
            List(new Variable(name, state, theType))
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
    
    val newVar = new Variable(name.getRawSignature, State.this, fcnType)
    setValue(functionCount, newVar.address)
    
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

    functionList.find(_.name == name).map{ fcn =>
      if (!fcn.isNative) {

        functionContexts.push(new ExecutionContext(List(), functionContexts.head.varMap, call.getExpressionType, stackInsertIndex, this))

        val resolvedArgs = args.map{x =>
          Utils.allocateString(x, false)(State.this)
        }

        // this is a function simulated in scala
        val returnVal = fcn.run(resolvedArgs.reverse, this)

        popFunctionContext

        returnVal.foreach{ retVal =>
          context.stack.push(RValue(retVal, null))
        }

        Seq()
      } else {
        callFunction(fcn, call, args)
        Seq(fcn.node)
      }
    }.getOrElse{
      // function pointer case
      val fcnPointer = functionContexts.head.varMap.find{_.name == name}.get
      val fcn = getFunctionByIndex(fcnPointer.value.asInstanceOf[Int])
      Seq(fcn.node)
    }
  }
  
  def callFunction(function: Function, call: IASTFunctionCallExpression, args: Array[ValueType]): IASTNode = {
    functionContexts.push(new ExecutionContext(function.staticVars, functionContexts.head.varMap, call.getExpressionType, stackInsertIndex, this))
    context.pathStack.push(call)

    args.foreach{ arg => context.stack.push(arg)}
    context.stack.push(RValue(args.size, null))

    function.node
  }

  def clearVisited(parent: IASTNode) {
    context.visited -= parent
    parent.getChildren.foreach { node =>
      clearVisited(node)
    }
  }

  def allocateSpace(numBytes: Int): Int = {
    if (numBytes > 0) {
      val result = stackInsertIndex
      stackInsertIndex += numBytes
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
         tape.put(dst + i, tape.get(src + i))
      }   
    }
  }
  
  def readPtrVal(address: Int): Int = {
    readVal(address, TypeHelper.pointerType).value.asInstanceOf[Int]
  }
  
  def createStringVariable(str: String, isHeap: Boolean)(implicit state: State): RValue = {
    val theStr = Utils.stripQuotes(str)
    val translateLineFeed = theStr.replace("\\n", 10.asInstanceOf[Char].toString)
    val withNull = (translateLineFeed.toCharArray() :+ 0.toChar).map{char => RValue(char.toByte, new CBasicType(IBasicType.Kind.eChar, 0))} // terminating null char
    val strAddr = if (isHeap) allocateHeapSpace(withNull.size) else allocateSpace(withNull.size)
    
    setArray(withNull, strAddr, 1)
    RValue(strAddr, TypeHelper.pointerType)
  }

  def readVal(address: Int, theType: IType): RValue = {

    import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._
    
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
      case qual: IQualifierType => readVal(address, qual.getType).value
      case typedef: CTypedef => readVal(address, typedef.getType).value
    }
    
    TypeHelper.castSign(theType, result)
  }

  def setArray(array: Array[RValue], address: Int, stride: Int): Unit = {
      var i = 0
      array.foreach { element =>
        element match {
          case RValue(newVal, _) =>
            setValue(newVal, address + i)
        }

        i += stride
      }
  }

  // use Address type to prevent messing up argument order
  def setValue(newVal: AnyVal, address: Int): Unit = newVal match {
    case char: char    => tape.put(address, char)
    case long: Long => tape.putInt(address, long.toInt)
    case short: Short  => tape.putShort(address, short)
    case bool: Boolean => tape.putInt(address, if (bool) 1 else 0)
    case int: Int => tape.putInt(address, int)
    case float: Float   => tape.putFloat(address, float)
    case double: Double  => tape.putDouble(address, double)
  }
}