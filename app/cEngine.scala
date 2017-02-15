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
      Gcc.runCode(sc.parts.iterator.next.asInstanceOf[String])
    }
  }
}

//abstract case class FunctionInfo(name: String, index: Int) {
//  def run: IASTNode
//}

class State(val tUnit: IASTTranslationUnit) {

  // turing tape 
  private val tape = ByteBuffer.allocate(100000);
  tape.order(ByteOrder.LITTLE_ENDIAN)

  var insertIndex = 0
  
  val functionContexts = new Stack[ExecutionContext]()
  def context = functionContexts.head
  val functionList = new ListBuffer[Function]()
  val functionPointers = scala.collection.mutable.Map[String, Variable]()
  val stdout = new ListBuffer[String]()
  var functionCount = 0
  def stack = context.stack
  
  def hasFunction(name: String): Boolean = functionList.exists{fcn => fcn.name == name}
  def getFunction(name: String): Function = functionList.find{fcn => fcn.name == name}.get
  def getFunctionByIndex(index: Int): Function = functionList.find{fcn => fcn.index == index}.get
  
  Functions.scalaFunctions.foreach{fcn =>
    addScalaFunctionDef(fcn)
  }
  
  def addScalaFunctionDef(fcn: Function) = {

    functionList += new Function(fcn.name, functionCount, false) {
      def run(args: Array[AnyVal], state: State): IASTNode = fcn.run(args, state)
    }
    
    val fcnType = new CFunctionType(new CBasicType(IBasicType.Kind.eVoid, 0), null)
    
    val newVar = new Variable(State.this, fcnType)
    newVar.allocate
    setValue(functionCount, newVar.address)
    
    functionPointers += fcn.name -> newVar
    functionCount += 1
  }
  
  def addFunctionDef(fcnDef: IASTFunctionDefinition) = {
    val name = fcnDef.getDeclarator.getName
    
    functionList += new Function(name.getRawSignature, functionCount, true) {
      def run(formattedOutputParams: Array[AnyVal], state: State): IASTNode = fcnDef
    }
    
    val fcnType = fcnDef.getDeclarator.getName.resolveBinding().asInstanceOf[IFunction].getType
    
    val newVar = new Variable(State.this, fcnType)
    newVar.allocate
    setValue(functionCount, newVar.address)
    
    functionPointers += name.getRawSignature -> newVar
    functionCount += 1
  }

  // flags
  var isReturning = false
  var isBreaking = false
  var isContinuing = false

  def callTheFunction(name: String, call: IASTFunctionCallExpression, args: Array[AnyVal]): Seq[IASTNode] = {
    functionList.find(_.name == name).map{ fcn =>
      if (!fcn.isNative) {
        fcn.run(args, this)
        Seq()
      } else {
        Seq(callFunction(name, call, args))
      }
    }.getOrElse{
      // function pointer case
      Seq(callFunctionPointer(name, call, args))
    }
  }
  
  def callFunction(name: String, call: IASTFunctionCallExpression, args: Seq[AnyVal]): IASTNode = {
        
    functionContexts.push(new ExecutionContext(functionContexts.head.varMap, call.getExpressionType, this))
    context.pathStack.push(call)

        // load up the stack with the arguments
    args.reverse.foreach { arg => context.stack.push(arg)}

    getFunction(name).run(args.toArray, this)
  }
  
  def callFunctionPointer(name: String, call: IASTFunctionCallExpression, args: Seq[AnyVal]): IASTNode = {
        
    functionContexts.push(new ExecutionContext(functionContexts.head.varMap, call.getExpressionType, this))
    context.pathStack.push(call)

        // load up the stack with the arguments
    args.reverse.foreach { arg => context.stack.push(arg)}

    val theVar = functionContexts.head.varMap(name)
    getFunctionByIndex(theVar.value.value.asInstanceOf[Int]).run(args.toArray, this)
  }

  def clearVisited(parent: IASTNode) {
    context.visited -= parent
    parent.getChildren.foreach { node =>
      clearVisited(node)
    }
  }

  def allocateSpace(numBytes: Int): Address = {
    if (numBytes > 0) {
      val result = insertIndex
      insertIndex += numBytes
      Address(result)
    } else {
      Address(0)
    }
  }
  
  def move(dst: Address, src: Address, numBytes: Int) = {
    if (numBytes != 0) {
      val array = Array.fill(numBytes)(0.toByte)
      val bytes = tape.get(array, src.value, numBytes)
      tape.put(array, dst.value, numBytes)
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
          tape.getShort(address)
        }  else if (basic.getKind == eInt && basic.isLong) {
          tape.getLong(address)
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
      case qual: IQualifierType => readVal(addr, qual.getType).value
      case typedef: CTypedef => readVal(addr, typedef.getType).value
    }
    
    TypeHelper.cast(theType, result)
  }

  def putPointer(addr: Address, newVal: AnyVal) = {
    tape.putInt(addr.value, newVal.asInstanceOf[Int])
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
    case char: Character    => tape.put(address.value, char)
    case long: Long => tape.putLong(address.value, long)
    case short: Short  => tape.putShort(address.value, short) 
    case int: Int => tape.putInt(address.value, int) 
    case Address(int) => tape.putInt(address.value, int) 
    case float: Float   => tape.putFloat(address.value, float)
    case double: Double  => tape.putDouble(address.value, double)
  }
}