package scala.c.engine

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

abstract class ValueType {
  def theType: IType
}

abstract class LValue(state: State) extends ValueType {
  val address: Int
  val theType: IType
  def sizeof: Int
  def value: RValue

  override def toString = {
    "AddressInfo(" + address + ", " + theType + ")"
  }
}

case class StringLiteral(value: String) extends ValueType {
  val theType = new CPointerType(new CBasicType(IBasicType.Kind.eChar, 0), 0)
}

case class TypeInfo(value: IType) extends ValueType {
  val theType = value
}

case class RValue(value: AnyVal, theType: IType) extends ValueType {
  override def toString = {
    "RValue(" + value + ", " + theType + ")"
  }
}

case class Field(state: State, address: Int, theType: IType, size: Int) extends LValue(state) {
  val sizeof = size
  def value = state.Stack.readFromMemory(address, theType)
}

case class MemoryLocation(state: State, address: Int, theType: IType) extends LValue(state) {
  val sizeof = TypeHelper.sizeof(theType)
  def value = state.Stack.readFromMemory(address, theType)
}

object LValue {
  def unapply(info: LValue): Option[(Int, IType)] = Some((info.address, info.theType))
}

case class Variable(name: String, state: State, theType: IType, dim: Seq[Int]) extends LValue(state) {

  def setArray(array: Array[RValue]): Unit = {
    state.setArray(array, address, TypeHelper.sizeof(theType))
  }

  def allocateSpace(state: State, aType: IType, dimensions: Seq[Int] = Seq()): Int = {
    def recurse(aType: IType, dimensions: Seq[Int]): Int = aType match {
      case array: IArrayType =>
        if (dimensions.size > 0) { // need this guard
          if (array.getType.isInstanceOf[IArrayType]) { // multi-dimensional array
            val addr = state.allocateSpace(TypeHelper.sizeof(TypeHelper.pointerType) * dimensions.head)
            for (i <- (0 until dimensions.head)) {
              val subaddr = recurse(array.getType, dimensions.tail)
              state.Stack.writeToMemory(subaddr, addr + i * 4, TypeHelper.pointerType) // write pointer
            }
            addr
          } else {
            state.allocateSpace(TypeHelper.sizeof(array.getType) * dimensions.head)
          }
        } else {
          val size = if (array.hasSize) {
            array.getSize.numericalValue().toInt
          } else {
            1
          }

          if (size > 0) {
            (0 until size).map { _ =>
              recurse(array.getType, dimensions)
            }.head
          } else {
            0
          }
        }
      case array: IPointerType =>
        state.allocateSpace(TypeHelper.sizeof(TypeHelper.pointerType))
      case fcn: CFunctionType =>
        state.allocateSpace(TypeHelper.sizeof(TypeHelper.pointerType))
      case struct: CStructure =>
        struct.getKey match {
          case ICompositeType.k_struct =>
            struct.getFields.map { field =>
              recurse(field.getType, dimensions)
            }.head
          case ICompositeType.k_union =>
            var maxSize = 0
            struct.getFields.foreach { field =>
              val size = TypeHelper.sizeof(field)
              if (size > maxSize) {
                maxSize = size
              }
            }
            state.allocateSpace(maxSize)
        }
      case typedef: CTypedef =>
        recurse(typedef.asInstanceOf[CTypedef].getType, dimensions)
      case qual: IQualifierType =>
        recurse(qual.asInstanceOf[IQualifierType].getType, dimensions)
      case basic: IBasicType =>
        state.allocateSpace(TypeHelper.sizeof(basic))
    }

    recurse(aType, dimensions)
  }

  val (allocate, theSize) = {
    val current = state.Stack.insertIndex
    val result = allocateSpace(state, theType, dim.reverse)
    val size = state.Stack.insertIndex - current
    (result, size)
  }

  val address = allocate

  def sizeof = theSize

  // need this for function-scoped static vars
  var isInitialized = false

  def value = if (!dim.isEmpty) {
    RValue(address, theType)
  } else {
    state.Stack.readFromMemory(address, theType)
  }

  override def toString = {
    "Variable(" + name + ", " + address + ", " + theType + ")"
  }

  def setValues(values: List[ValueType]) = {
    var offset = 0
    values.foreach {
      case RValue(value, theType) =>
        state.Stack.writeToMemory(value, address + offset, theType)
        offset += TypeHelper.sizeof(theType)
    }
  }
}
