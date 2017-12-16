package scala.c.engine

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

abstract class ValueType {
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
      def value = state.Stack.readFromMemory(address, theType)
    }
  }
}

class ArrayVariable(name: String, state: State, arrayType: IArrayType, dim: Seq[Int]) extends Variable(name, state, arrayType) {

  override val theType = arrayType

  override def value: RValue = RValue(address, theType)

  val allocate: Int = {
    // where we store the actual data

    def recurse(subType: IType, dimensions: Seq[Int]): Int = {

      if (dimensions.size > 0) { // need this guard
        subType match {
          case array: IArrayType if array.getType.isInstanceOf[IArrayType] => // multi-dimensional array
            val addr = state.allocateSpace(TypeHelper.sizeof(TypeHelper.pointerType) * dimensions.head)
            for (i <- (0 until dimensions.head)) {
              val subaddr = recurse(array.getType, dimensions.tail)
              state.Stack.writeToMemory(subaddr, addr + i * 4, TypeHelper.pointerType) // write pointer
            }
            addr
          case array: IArrayType =>
            state.allocateSpace(TypeHelper.sizeof(array.getType) * dimensions.head)
        }
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
  val address = allocateSpace(state, aType)

  override def hashCode: Int = {
    name.hashCode
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case aVar: Variable => aVar.name == name
      case x => x.equals(this)
    }
  }

  // need this for function-scoped static vars
  var isInitialized = false

  def value = state.Stack.readFromMemory(address, theType)

  override def toString = {
    "Variable(" + name + ", " + address + ", " + theType + ")"
  }

  def allocateSpace(state: State, aType: IType): Int = {
    def recurse(aType: IType): Int = aType match {
      case array: IArrayType =>
        val size = if (array.hasSize) {
          array.getSize.numericalValue().toInt
        } else {
          1
        }

        if (size > 0) {
          (0 until size).map { _ =>
            recurse(array.getType)
          }.head
        } else {
          0
        }
      case array: IPointerType =>
        state.allocateSpace(TypeHelper.sizeof(TypeHelper.pointerType))
      case fcn: CFunctionType =>
        state.allocateSpace(TypeHelper.sizeof(TypeHelper.pointerType))
      case struct: CStructure =>
        struct.getKey match {
          case ICompositeType.k_struct =>
            struct.getFields.map { field =>
              recurse(field.getType)
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
        recurse(typedef.asInstanceOf[CTypedef].getType)
      case qual: IQualifierType =>
        recurse(qual.asInstanceOf[IQualifierType].getType)
      case basic: IBasicType =>
        state.allocateSpace(TypeHelper.sizeof(basic))
    }

    recurse(aType)
  }

//  private def allocateFieldSpace(state: State, aType: IType): Int = {
//    def recurse(aType: IType): Int = aType match {
//      case array: IArrayType =>
//        state.allocateSpace(TypeHelper.sizeof(TypeHelper.pointerType) * array.getSize.numericalValue().toInt)
//      case array: IPointerType =>
//        state.allocateSpace(TypeHelper.sizeof(TypeHelper.pointerType))
//      case fcn: CFunctionType =>
//        state.allocateSpace(TypeHelper.sizeof(TypeHelper.pointerType))
//      case structType: CStructure =>
//        val struct = structType.asInstanceOf[CStructure]
//        var result = -1
//        struct.getKey match {
//          case ICompositeType.k_struct =>
//            struct.getFields.foreach { field =>
//              if (result == -1) {
//                result = allocateFieldSpace(state, field.getType)
//              } else {
//                recurse(field.getType)
//              }
//            }
//          case ICompositeType.k_union =>
//            var maxSize = 0
//            struct.getFields.foreach { field =>
//              val size = TypeHelper.sizeof(field.getType)
//              if (size > maxSize) {
//                maxSize = size
//              }
//            }
//            result = state.allocateSpace(maxSize)
//        }
//        result
//      case typedef: CTypedef =>
//        recurse(typedef.asInstanceOf[CTypedef].getType)
//      case qual: IQualifierType =>
//        recurse(qual.asInstanceOf[IQualifierType].getType)
//      case basic: IBasicType =>
//        state.allocateSpace(TypeHelper.sizeof(basic))
//    }
//
//    recurse(aType)
//  }

  def setValues(values: List[ValueType]) = {
    var offset = 0
    values.foreach {
      case RValue(value, theType) =>
        state.Stack.writeToMemory(value, address + offset, theType)
        offset += TypeHelper.sizeof(theType)
    }
  }
}
