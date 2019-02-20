package scala.c.engine

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

abstract class ValueType {
  def theType: IType
}

trait LValue extends ValueType {
  val address: Int
  val theType: IType
  val bitOffset: Int
  val state: State
  val sizeInBits: Int

  def sizeof: Int
  def rValue: RValue = {
    if (TypeHelper.isPointer(this)) {
      Address(getValue.value.asInstanceOf[Int], TypeHelper.getPointerType(theType))
    } else {
      RValue(getValue.value, theType)
    }
  }

  def getValue = if (theType.isInstanceOf[IArrayType]) {
    RValue(address, theType)
  } else {
    state.Stack.readFromMemory(address, theType, bitOffset, sizeInBits)
  }

  //protected def getValue = state.Stack.readFromMemory(address, theType, bitOffset, sizeInBits)
  def setValue(newVal: AnyVal) = {
    state.Stack.writeToMemory(newVal, address, theType, bitOffset, sizeInBits)
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
      //def sizeof = TypeHelper.sizeof(theType)(state)}
      val sizeof = {
        if (TypeHelper.isPointer(theType)) {
          val nestedSize = TypeHelper.sizeof(TypeHelper.getPointerType(theType))(theState)

          val result = theType match {
            case array: IArrayType => nestedSize * array.getSize.numericalValue().toInt
            case _ => nestedSize
          }

          result
        } else {
          TypeHelper.sizeof(theType)(state)
        }
      }
    }
}

case class StringLiteral(value: String) extends ValueType {
  val theType = new CPointerType(new CBasicType(IBasicType.Kind.eChar, 0), 0)
}

case class TypeInfo(value: IType) extends ValueType {
  val theType = value
}

object RValue {
  def unapply(rvalue: RValue): Option[(AnyVal, IType)] = Some((rvalue.value, rvalue.theType))
  def apply(theValue: AnyVal, aType: IType) =
    new RValue {val theType = TypeHelper.stripSyntheticTypeInfo(aType); val value = theValue;}
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
}

case class Field(state: State, address: Int, bitOffset: Int, theType: IType, sizeInBits: Int) extends LValue {
  val sizeof = sizeInBits / 8
}

case class Variable(name: String, state: State, aType: IType) extends LValue {

  val theType = TypeHelper.stripSyntheticTypeInfo(aType)
  val bitOffset = 0
  val sizeInBits = sizeof * 8

  def setArray(array: Array[RValue]): Unit = {
    def recursiveWrite(aType: IType, theAddress: Int, values: Array[RValue]): Unit = aType match {
      case theArray: IArrayType =>

        if (theArray.hasSize) {
          if (theArray.getType.isInstanceOf[IArrayType]) { // e.g int blah[2][3]
            val dataStart = state.readPtrVal(theAddress)
            state.writeDataBlock(values, dataStart)(state)
          } else {
            state.writeDataBlock(values, theAddress)(state)
          }
        } else {
          if (theArray.getType.isInstanceOf[IArrayType]) { // e.g int blah[][3]
            val size = theArray.getType.asInstanceOf[CArrayType].getSize.numericalValue().toInt
            for (i <- (0 until size)) {
              val dataStart = state.readPtrVal(theAddress + i * 4)
              val result = values.drop(i * size)
              recursiveWrite(theArray.getType, dataStart, result.take(size))
            }
          } else {
            state.writeDataBlock(values, theAddress)(state)
          }
        }
      case ptr: CPointerType => state.writeDataBlock(values, theAddress)(state)
    }

    recursiveWrite(aType, address, array)
  }

  def allocateSpace(state: State, aType: IType): Int = {
    def recurse(aType: IType): Int = aType match {
      case array: IArrayType =>

        val size = if (array.hasSize) {
          array.getSize.numericalValue().toInt
        } else {
          1
        }

        if (array.getType.isInstanceOf[IArrayType]) { // multi-dimensional array
          val addr = state.allocateSpace(TypeHelper.sizeof(array)(state) * size)
          for (i <- (0 until size)) {
            val subaddr = recurse(array.getType)
            state.Stack.writeToMemory(subaddr, addr + i * 4, state.pointerType) // write pointer
          }
          addr
        } else {
          state.allocateSpace(TypeHelper.sizeof(array.getType)(state) * size)
        }
      case x =>
        state.allocateSpace(TypeHelper.sizeof(x)(state))
    }

    recurse(aType)
  }

  val (allocate, theSize) = {
    val current = state.Stack.insertIndex
    val result = allocateSpace(state, theType)
    val size = state.Stack.insertIndex - current
    (result, size)
  }

  val address = allocate

  def sizeof = theSize

  // need this for function-scoped static vars
  var isInitialized = false

  override def toString = {
    "Variable(" + name + ", " + address + ", " + theType + ")"
  }
}
