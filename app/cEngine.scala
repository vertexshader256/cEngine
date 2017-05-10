package app.astViewer

import org.eclipse.cdt.core.dom.ast._
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import java.nio.ByteBuffer
import java.nio.ByteOrder
import org.eclipse.cdt.internal.core.dom.parser.c._

object cEngine {
  type JSONObject = Any
  
  implicit class CounterSC(val sc: StringContext) extends AnyVal {
    // Define functions that we want to use with string interpolation syntax
    def c(args: Any*)(implicit state: State): Unit = {
      Gcc.runCode(sc.parts.iterator.next.asInstanceOf[String], state)
    }
  }
}

//abstract case class FunctionInfo(name: String, index: Int) {
//  def run: IASTNode
//}

class State {

  var tUnit: IASTTranslationUnit = null
  var current: IASTNode = null
  var direction: Direction = Entering

  // turing tape 
  private val tape = ByteBuffer.allocate(100000);
  tape.order(ByteOrder.LITTLE_ENDIAN)

  var stackInsertIndex = 0
  var heapInsertIndex = 50000
  
  val functionContexts = new Stack[ExecutionContext]()
  def context = functionContexts.head
  val functionList = new ListBuffer[Function]()
  val functionPointers = scala.collection.mutable.Map[String, Variable]()
  val stdout = new ListBuffer[String]()
  var functionCount = 0
  def stack = context.stack
  
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
    
    val newVar = new Variable(State.this, fcnType)
    setValue(functionCount, newVar.address)
    
    functionPointers += fcn.name -> newVar
    functionCount += 1
  }
  
  def addFunctionDef(fcnDef: IASTFunctionDefinition) = {
    val name = fcnDef.getDeclarator.getName
    
    val fcnType = fcnDef.getDeclarator.getName.resolveBinding().asInstanceOf[IFunction].getType

    functionList += new Function(name.getRawSignature, true) {
      index = functionCount
      def parameters = fcnType.getParameterTypes.toList
      def run(formattedOutputParams: Array[ValueInfo], state: State): Option[AnyVal] = {None}
      override def getNext = fcnDef
    }
    
    val newVar = new Variable(State.this, fcnType)
    setValue(functionCount, newVar.address)
    
    functionPointers += name.getRawSignature -> newVar
    functionCount += 1
  }

  // flags
  var isReturning = false
  var isBreaking = false
  var isContinuing = false

  def callFunctionFromScala(name: String, args: Array[ValueInfo]): Seq[IASTNode] = {

    functionList.find(_.name == name).map { fcn =>
      // this is a function simulated in scala
      fcn.run(args.reverse, this).foreach { retVal => context.stack.push(ValueInfo(retVal, null)) }
    }

    Seq()
  }

  def callTheFunction(name: String, call: IASTFunctionCallExpression, args: Array[ValueInfo]): Seq[IASTNode] = {

    functionList.find(_.name == name).map{ fcn =>
      if (!fcn.isNative) {
        // this is a function simulated in scala
        fcn.run(args.reverse, this).foreach{retVal => context.stack.push(ValueInfo(retVal, null))}
        Seq()
      } else {
        callFunction(fcn, call, args)
        Seq(fcn.getNext)
      }
    }.getOrElse{
      // function pointer case
      val fcnPointer = functionContexts.head.varMap(name)
      val fcn = getFunctionByIndex(fcnPointer.value.asInstanceOf[Int])
      Seq(fcn.getNext)
    }
  }
  
  def callFunction(function: Function, call: IASTFunctionCallExpression, args: Array[ValueInfo]): IASTNode = {
    functionContexts.push(new ExecutionContext(functionContexts.head.varMap, call.getExpressionType, stackInsertIndex, this))
    context.pathStack.push(call)
    
    args.foreach{ arg => context.stack.push(arg)}
    context.stack.push(ValueInfo(args.size, null))

    function.getNext
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
  
  def readPtrVal(address: Int): ValueInfo = {
    readVal(address, TypeHelper.pointerType)
  }
  
  def createStringVariable(str: String, isHeap: Boolean)(implicit state: State): ValueInfo = {
    val theStr = Utils.stripQuotes(str)
    val translateLineFeed = theStr.replace("\\n", 10.asInstanceOf[Char].toString)
    val withNull = (translateLineFeed.toCharArray() :+ 0.toChar).map{char => ValueInfo(char.toByte, new CBasicType(IBasicType.Kind.eChar, 0))} // terminating null char
    val strAddr = if (isHeap) allocateHeapSpace(withNull.size) else allocateSpace(withNull.size)
    
    setArray(withNull, AddressInfo(strAddr, new CBasicType(IBasicType.Kind.eChar, 0)))
    ValueInfo(strAddr, TypeHelper.pointerType)
  }

  def readVal(address: Int, theType: IType): ValueInfo = {

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
      case array: IArrayType => tape.getInt(address)
      case fcn: IFunctionType => tape.getInt(address)
      case struct: CStructure => tape.getInt(address)
      case qual: IQualifierType => readVal(address, qual.getType).value
      case typedef: CTypedef => readVal(address, typedef.getType).value
    }
    
    TypeHelper.castSign(theType, result)
  }

  def setArray(array: Array[ValueInfo], info: AddressInfo)(implicit state: State): Unit = {
      var i = 0
      val address = info.address
      val size = TypeHelper.sizeof(info.theType)
      array.foreach { element =>
        element match {
          case ValueInfo(newVal, _) =>
            setValue(newVal, address + i)
        }

        i += size
      }
  }
  
  def resolve(theType: IType): IType = {
    val result = theType match {
      case ptr: IPointerType => ptr.getType
      case typedef: ITypedef => typedef.getType
      case array: IArrayType => array.getType
      case qual: IQualifierType => qual.getType
      case _ => TypeHelper.resolve(theType)
    }
    
    if (result.isInstanceOf[IQualifierType] || result.isInstanceOf[ITypedef]) {
      resolve(result)
    } else {
      result
    }
  }
  
  // use Address type to prevent messing up argument order
  def setValue(newVal: AnyVal, address: Int): Unit = newVal match {
    case char: Character    => tape.put(address, char)
    case long: Long => tape.putInt(address, long.toInt)
    case short: Short  => tape.putShort(address, short)
    case bool: Boolean => tape.putInt(address, if (bool) 1 else 0)
    case int: Int => tape.putInt(address, int)
    case float: Float   => tape.putFloat(address, float)
    case double: Double  => tape.putDouble(address, double)
  }
}