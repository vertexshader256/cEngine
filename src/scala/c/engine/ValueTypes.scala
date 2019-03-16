package scala.c.engine

import java.io.File
import java.nio.file.{Files, Paths}

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

abstract class ValueType {
  def theType: IType
  def rawType: IType
}

trait LValue extends ValueType {
  val address: Int
  val theType: IType
  val bitOffset: Int
  val state: State
  val sizeInBits: Int

  private var rVal: RValue = RValue(0, TypeHelper.intType)

  def sizeof: Int
  def rValue: RValue = {
    if (rVal.isInstanceOf[FileRValue]) {
      rVal
    } else if (TypeHelper.isPointerOrArray(this)) {
      Address(getValue.value.asInstanceOf[Int], TypeHelper.getPointerType(theType))
    } else {
      RValue(getValue.value, theType)
    }
  }

  private def getValue = if (theType.isInstanceOf[IArrayType]) {
    RValue(address, theType)
  } else {
    state.Stack.readFromMemory(address, theType, bitOffset, sizeInBits)
  }

  def setValue(newVal: RValue) = {
    rVal = newVal
    state.Stack.writeToMemory(newVal.value, address, theType, bitOffset, sizeInBits)
  }

  def toByteArray = state.readDataBlock(address, sizeof)(state)
}

object LValue {
  def unapply(info: LValue): Option[(Int, IType)] = Some((info.address, info.theType))
  def apply(theState: State, addr: Int, aType: IType) =
    new LValue {
      val address = addr
      val state = theState
      val bitOffset = 0
      val sizeInBits = sizeof * 8
      val theType = TypeHelper.stripSyntheticTypeInfo(aType)
      val rawType = aType
      //def sizeof = TypeHelper.sizeof(theType)(state)}
      val sizeof = {
        TypeHelper.getPointerSize(theType)(state)
      }
    }
}

case class StringLiteral(value: String) extends ValueType {
  val theType = new CPointerType(new CBasicType(IBasicType.Kind.eChar, 0), 0)
  val rawType = theType
}

case class TypeInfo(value: IType) extends ValueType {
  val theType = value
  val rawType = theType
}

object RValue {
  def unapply(rvalue: RValue): Option[(AnyVal, IType)] = Some((rvalue.value, rvalue.theType))
  def apply(theValue: AnyVal, aType: IType) =
    new RValue {val theType = TypeHelper.stripSyntheticTypeInfo(aType); val rawType = aType; val value = theValue;}
  def apply(theValue: AnyVal) =
    new RValue {val theType = null; val rawType = null; val value = theValue;}
}

abstract class RValue extends ValueType {
  val value: AnyVal
  val theType: IType

  override def toString = {
    "RValue(" + value + ", " + theType + ")"
  }
}

case class Address(value: Int, theType: IType) extends RValue {
  override def toString = {
    "Address(" + value + ", " + theType + ")"
  }
  val rawType = theType
}

case class FileRValue(path: String) extends RValue {
  val value: AnyVal = 0
  val theType = null
  val rawType = theType
  //val file: File = new File(path)
  val byteArray = Files.readAllBytes(Paths.get(path))
  var currentPosition = 0
  def fread(numBytes: Int): Array[Byte] = {
    val result = byteArray.drop(currentPosition).take(numBytes)
    currentPosition += numBytes
    result
  }
}

case class Field(state: State, address: Int, bitOffset: Int, theType: IType, sizeInBits: Int) extends LValue {
  val sizeof = sizeInBits / 8
  val rawType = theType
}

object Variable {
  def apply(name: String, state: State, aType: IType, initVals: List[RValue]): Variable = {

    val typeSize = TypeHelper.sizeof(aType)(state)
    val initValSize = initVals.map{init => TypeHelper.sizeof(init.theType)(state)}.sum

    val size = Math.max(typeSize, initValSize)
    val variable = Variable(name: String, state: State, aType: IType, size)

    // first clear the memory
    val zeroArray = (0 until TypeHelper.sizeof(aType)(state)).map{x => 0.toByte}.toArray
    state.writeDataBlock(zeroArray, variable.address)(state)

    // now, write the initial values
    state.writeDataBlock(initVals, variable.address)(state)
    variable
  }

  def apply(name: String, state: State, aType: IType): Variable = {
    val size = TypeHelper.sizeof(aType)(state)
    Variable(name: String, state: State, aType: IType, size)
  }
}

case class Variable(name: String, state: State, aType: IType, sizeof: Int) extends LValue {

  val theType = TypeHelper.stripSyntheticTypeInfo(aType)
  val rawType = aType
  val bitOffset = 0
  val sizeInBits = sizeof * 8

  val address = state.allocateSpace(sizeof)

  // need this for function-scoped static vars
  var isInitialized = false

  override def toString = {
    "Variable(" + name + ", " + address + ", " + theType + ")"
  }
}
