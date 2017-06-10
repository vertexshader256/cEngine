package c.engine

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

trait ValueType {
  def theType: IType
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

abstract class LValue(state: State) extends ValueType {
  val address: Int
  val theType: IType
  def sizeof = TypeHelper.sizeof(theType)
  def value: RValue

  override def toString = {
    "AddressInfo(" + address + ", " + theType + ")"
  }
}

object LValue {
  def unapply(info: LValue): Option[(Int, IType)] = Some((info.address, info.theType))

  def apply(addr: Int, aType: IType)(implicit state: State) = {
    new LValue(state) {
      val address = addr
      val theType = aType
      def value = state.readVal(address, theType)
    }
  }
}

class ArrayVariable(name: String, state: State, arrayType: IArrayType, dim: Seq[Int]) extends Variable(name, state, arrayType) {

  override val theType = arrayType

  override def value: RValue = RValue(address, theType)

  val allocate: Int = {
    // where we store the actual data

    def recurse(subType: IArrayType, dimensions: Seq[Int]): Int = {

      if (dimensions.size > 0) {
        val addr = allocateSpace(state, subType.getType, dimensions.head)
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

  override val address = allocate

  def setArray(array: Array[RValue]): Unit = {
    state.setArray(array, address, TypeHelper.sizeof(theType))
  }

  override def sizeof: Int = {
    val numElements = if (dim.isEmpty) 0 else dim.reduce{_ * _}
    TypeHelper.sizeof(theType) * numElements
  }
}

class Variable(val name: String, val state: State, aType: IType) extends LValue(state) {
  val theType = aType
  val size = TypeHelper.sizeof(aType)
  val address = allocateSpace(state, aType, 1)

  // need this for function-scoped static vars
  var isInitialized = false

  def value = state.readVal(address, theType)

  override def toString = {
    "Variable(" + name + ", " + address + ", " + theType + ")"
  }

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

  def setValues(values: List[ValueType]) = {
    var offset = 0
    values.foreach {
      case RValue(value, theType) =>
        state.setValue(value, address + offset)
        offset += TypeHelper.sizeof(theType)
    }
  }
}
