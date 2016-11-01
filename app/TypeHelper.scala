package app.astViewer

import org.eclipse.cdt.internal.core.dom.parser.c._
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._
import org.eclipse.cdt.core.dom.ast._

object TypeHelper {
  
  val pointerType = new CBasicType(IBasicType.Kind.eInt , 0)
  
  // casts 'newVal' to 'theType'
  def cast(theType: IBasicType, newVal: AnyVal): Primitive = {
    val casted: AnyVal = theType.getKind match {
      case `eChar` if theType.isUnsigned    => 
        newVal match {
          case int: Int => int.toChar.toByte.toChar & 0xFF
          case char: Char => char.toByte.toChar & 0xFF
          case byte: Byte => byte.toChar & 0xFF
        } 
      case `eChar`    => 
        newVal match {
          case int: Int => int.toChar.toByte.toChar
          case char: Char => char.toByte.toChar
          case byte: Byte => byte.toChar
        } 
     case `eInt` if theType.isLong && theType.isUnsigned =>
        newVal match {
          case int: Int => int.toLong & 0x00000000ffffffffL
          case long: Long => long & 0x00000000ffffffffL
        } 
     case `eInt` if theType.isLong =>
        newVal match {
          case int: Int => int.toLong
          case long: Long => long
        } 
     case `eInt` if theType.isShort & theType.isUnsigned =>
        newVal match {
          case int: Int => int.toShort & 0xFFFF
          case short: Short => short & 0xFFFF
          case long: Long => long.toShort & 0xFFFF
        }  
     case `eInt` if theType.isShort =>
        newVal match {
          case int: Int => int.toShort
          case short: Short => short
          case long: Long => long.toShort
        }  
     case `eInt`     => 
        newVal match {
          case boolean: Boolean => if (boolean) 1 else 0
          case long: Long => long.toInt
          case int: Int => int
          case short: Short => short.toInt
          case char: Char => char.toByte.toInt
          case double: Double => double.toInt
          case float: Float => float.toInt
        }  
     case `eFloat`   =>
        newVal match {
          case int: Int => int.toFloat
          case double: Double => double.toFloat
          case float: Float => float
        }  
     case `eDouble`  =>
        newVal match {
          case int: Int => int.toDouble
          case double: Double => double
          case float: Float => float.toDouble
        } 
      case `eBoolean` =>
        // booleans are integers in C
        val result: Int = newVal match {
          case bool: Boolean => 1
          case int: Int => if (int > 0) 1 else 0
        } 
        result
      case `eVoid` =>
        newVal match {
          case int: Int => int
          case double: Double => double
          case float: Float => float
          case char: Char => char
        } 
    }
    
    Primitive(casted, theType)
  }
  
  def isSigned(theType: IBasicType) = {
    theType.isSigned || (!theType.isSigned && !theType.isUnsigned)
  }
  
  // resolves 'Any' to 'AnyVal'
  def resolve(state: State, theType: IType, any: Any): Primitive = {
    Primitive(any match {
      case VarRef(name) =>
        state.vars.resolveId(name).value.value
      case lit @ Literal(_) => lit.typeCast(TypeHelper.resolve(theType)).value
      case AddressInfo(addy, _) => addy.value
      case Address(addy) => addy
      case Primitive(theVal, _) => theVal
      case int: Int => int
      case float: Float => float
      case double: Double => double
      case long: Long => long
      case boolean: Boolean => boolean
    }, TypeHelper.resolve(theType))
  }

  def resolve(theType: IType): IBasicType = theType match {
    case struct: CStructure       => new CBasicType(IBasicType.Kind.eInt, 0)
    case basicType: IBasicType    => basicType
    case typedef: ITypedef        => resolve(typedef.getType)
    case ptrType: IPointerType    => resolve(ptrType.getType)
    case arrayType: IArrayType    => resolve(arrayType.getType)
    case qualType: IQualifierType => resolve(qualType.getType)
  }

  def sizeof(theType: IType): Int = theType match {
    case ptr: IPointerType =>
      4
    case struct: CStructure =>
      struct.getFields.map { field =>
        sizeof(field.getType)
      }.sum
    case array: IArrayType =>
      sizeof(array.getType)
    case typedef: CTypedef =>
      sizeof(typedef.getType)
    case qual: IQualifierType =>
      sizeof(qual.getType)
    case basic: IBasicType =>
      basic.getKind match {
        case `eInt` if basic.isLong => 8
        case `eInt`                 => 4
        case `eFloat`               => 4
        case `eChar16`              => 2
        case `eDouble`              => 8
        case `eChar`                => 1
        case `eChar32`              => 4
        case `eVoid`                => 4
        case `eBoolean`             => 4
      }
  }
  
  def isPointer(theType: IType) = theType.isInstanceOf[IArrayType] || theType.isInstanceOf[IPointerType]
}