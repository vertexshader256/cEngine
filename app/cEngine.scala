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
      println(sc.parts.iterator.next.asInstanceOf[String])
      Gcc.runCode(sc.parts.iterator.next.asInstanceOf[String], state)
    }

  }
}

class State {
  
  val executionContext = new Stack[FunctionExecutionContext]()
  val globals = Map[String, RuntimeVariable]()
  var vars: FunctionExecutionContext = null
  val functionMap = scala.collection.mutable.Map[String, IASTNode]()
  val stdout = new ListBuffer[String]()

  // flags
  var isReturning = false
  var isBreaking = false
  var isContinuing = false
  var isPreprocessing = true
  
  def stack = {
    vars.stack
  }

  def callFunction(call: IASTFunctionCallExpression, args: Seq[Any]) = {
    executionContext.push(vars)

    vars = new FunctionExecutionContext(globals, call.getExpressionType)
    vars.pathStack.push(call)
    
        // load up the stack with the parameters
    args.reverse.foreach { arg => vars.stack.push(arg)}

    val name = call.getFunctionNameExpression match {
      case x: IASTIdExpression => x.getName.getRawSignature
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
    if (numBytes > 0) {
      val result = insertIndex
      insertIndex += numBytes
      Address(result)
    } else {
      Address(0)
    }
  }
  
  def readPtrVal(address: Address) = {
    readVal(address, TypeHelper.pointerType).value.asInstanceOf[Int]
  }
  
  def createStringVariable(str: String): Address = {
    val theStr = Utils.stripQuotes(str)
    val withNull = theStr.toCharArray() :+ 0.toChar // terminating null char
    val strAddr = allocateSpace(withNull.size)
    
    setArray(withNull, AddressInfo(strAddr, new CBasicType(IBasicType.Kind.eChar, 0)))
    strAddr
  }
  
  def readString(address: Address): String = {
     var current: Char = 0
      var stringBuilder = new ListBuffer[Char]()
      var i = 0
      do {
        current = readVal(address + i, new CBasicType(IBasicType.Kind.eChar, 0)).value.asInstanceOf[Byte].toChar
        if (current != 0) {
          stringBuilder += current
          i += 1
        }
      } while (current != 0)
        
      new String(stringBuilder.map(_.toByte).toArray, "UTF-8")
  }

  def readVal(addr: Address, theType: IType): ValueInfo = {

    import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._

    val address = addr.value
    
    val result: AnyVal = theType match {
      case basic: IBasicType =>
        // if it is neither signed or unsigned, assume its signed
        val isSigned = TypeHelper.isSigned(basic)
    
        if (basic.getKind == eInt && basic.isShort) {
          data.getShort(address)
        }  else if (basic.getKind == eInt && basic.isLong) {
          data.getLong(address)
        } else if (basic.getKind == eInt) {
          data.getInt(address)
        } else if (basic.getKind == eBoolean) {
          data.getInt(address)
        } else if (basic.getKind == eDouble) {
          data.getDouble(address)
        } else if (basic.getKind == eFloat) {
          data.getFloat(address)
        } else if (basic.getKind == eChar) {
          data.get(address) // a C 'char' is a Java 'byte'
        } else {
          throw new Exception("Bad read val")
        }
      case ptr: IPointerType => data.getInt(address)
      case array: IArrayType => data.getInt(address)
      case qual: IQualifierType => readVal(addr, qual.getType).value
      case typedef: CTypedef => readVal(addr, typedef.getType).value
    }
    
    TypeHelper.cast(theType, result)
  }

  def putPointer(addr: Address, newVal: AnyVal) = {
    data.putInt(addr.value, newVal.asInstanceOf[Int])
  }

  def setArray(array: Array[_], info: AddressInfo): Unit = {
      var i = 0
      val resolved = TypeHelper.resolve(info.theType)
      array.foreach { element =>
        element match {
          case addr @ Address(addy) => 
            setValue(addy, info.address + i)
          case lit @ Literal(_) =>
            setValue(lit.typeCast(resolved).value, info.address + i)
          case ValueInfo(newVal, _) =>
            setValue(newVal, info.address + i)
          case int: Int =>
            setValue(int, info.address + i)
          case char: Char =>
            setValue(char.toByte, info.address + i)
//          case double: Double =>
//            state.setValue(double, AddressInfo(theArrayAddress + i, resolved))
        }

        i += TypeHelper.sizeof(resolved)
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
  def setValue(newVal: AnyVal, address: Address): Unit = newVal match {
    case char: Character    => data.put(address.value, char)
    case long: Long => data.putLong(address.value, long)
    case short: Short  => data.putShort(address.value, short) 
    case int: Int => data.putInt(address.value, int) 
    case Address(int) => data.putInt(address.value, int) 
    case float: Float   => data.putFloat(address.value, float)
    case double: Double  => data.putDouble(address.value, double)
  }
}