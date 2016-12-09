package app.astViewer

import org.eclipse.cdt.internal.core.dom.parser.c._
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._
import org.eclipse.cdt.core.dom.ast._

object TypeHelper {
  
  val pointerType = new CBasicType(IBasicType.Kind.eInt , 0)
  
  // casts 'newVal' to 'theType'
  def cast(theType: IType, newVal: AnyVal): ValueInfo = {
    val casted: AnyVal = theType match {
      case typedef: CTypedef => cast(typedef.getType, newVal).value
      case qual: IQualifierType => cast(qual.getType, newVal).value
      case ptr: IPointerType => newVal.asInstanceOf[Int]
      case array: IArrayType => newVal.asInstanceOf[Int]
      case basic: IBasicType =>
       basic.getKind match {
          case `eChar` if basic.isUnsigned    => 
            newVal match {
              case int: Int => int & 0xFFFFFFFF
              case char: Character => char & 0xFF
            } 
          case `eChar`    => 
            newVal match {
              case int: Int => int.toChar.toByte
              case char: Character => char
              case char: Char => char.toByte
            } 
         case `eInt` if basic.isLong && basic.isUnsigned =>
            newVal match {
              case int: Int => int.toLong & 0x00000000ffffffffL
              case long: Long => long & 0x00000000ffffffffL
            } 
         case `eInt` if basic.isLong =>
            newVal match {
              case int: Int => int.toLong
              case long: Long => long
            } 
         case `eInt` if basic.isShort & basic.isUnsigned =>
            newVal match {
              case int: Int => int.toShort & 0xFFFF
              case short: Short => short & 0xFFFF
              case long: Long => long.toShort & 0xFFFF
            }  
         case `eInt` if basic.isShort =>
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
              case char: Character => char.toInt
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
              case char: Character => char
            } 
        }
      }
    
    ValueInfo(casted, theType)
  }
  
  // casts 'newVal' to 'theType'
  def downcast(theType: IType, newVal: AnyVal): ValueInfo = {
    val casted: AnyVal = theType match {
      case typedef: CTypedef => cast(typedef.getType, newVal).value
      case qual: IQualifierType => cast(qual.getType, newVal).value
      case ptr: IPointerType => if (newVal.isInstanceOf[Address]) newVal.asInstanceOf[Address] else newVal.asInstanceOf[Int]
      case array: IArrayType => if (newVal.isInstanceOf[Address]) newVal.asInstanceOf[Address] else newVal.asInstanceOf[Int]
      case basic: IBasicType =>
       basic.getKind match {
          case `eChar` if basic.isUnsigned    => 
            newVal match {
              case int: Int => (int & 0xFFFFFFFF).toChar.toByte
              case char: Character => char & 0xFF
            } 
          case `eChar`    => 
            newVal match {
              case int: Int => int.toChar.toByte
              case char: Character => char
              case char: Char => char.toByte
            } 
         case `eInt` if basic.isLong && basic.isUnsigned =>
            newVal match {
              case int: Int => int.toLong & 0x00000000ffffffffL
              case long: Long => long & 0x00000000ffffffffL
            } 
         case `eInt` if basic.isLong =>
            newVal match {
              case int: Int => int.toLong
              case long: Long => long
            } 
         case `eInt` if basic.isShort & basic.isUnsigned =>
            newVal match {
              case int: Int => int.toShort & 0xFFFF
              case short: Short => short & 0xFFFF
              case long: Long => long.toShort & 0xFFFF
            }  
         case `eInt` if basic.isShort =>
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
              case char: Character => char.toInt
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
              case char: Character => char
            } 
        }
      }
    
    ValueInfo(casted, theType)
  }
  
  def isSigned(theType: IBasicType) = {
    theType.isSigned || (!theType.isSigned && !theType.isUnsigned)
  }
  
  // resolves 'Any' to 'AnyVal'
  def resolve(theType: IType, any: Any)(implicit state: State): ValueInfo = {
    ValueInfo(any match {
      case Variable(value, _) =>
        value.value
      case lit @ Literal(_) => lit.typeCast(TypeHelper.resolve(theType)).value
      case AddressInfo(addy, _) => addy.value
      case Address(addy) => addy
      case ValueInfo(theVal, _) => theVal
      case int: Int => int
      case float: Float => float
      case double: Double => double
      case long: Long => long
      case boolean: Boolean => boolean
      case byte: Byte => byte
    }, TypeHelper.resolve(theType))
  }

  def resolve(theType: IType): IBasicType = theType match {
    case struct: CStructure       => TypeHelper.pointerType
    case basicType: IBasicType    => basicType
    case typedef: ITypedef        => resolve(typedef.getType)
    case ptrType: IPointerType    => resolve(ptrType.getType)
    case arrayType: IArrayType    => resolve(arrayType.getType)
    case qualType: IQualifierType => resolve(qualType.getType)
    case fcn: IFunctionType       => TypeHelper.pointerType
  }
  
  def resolve(op: Any)(implicit context: State): AnyVal = {
      op match {
        case lit @ Literal(_) => lit.cast.value
        case prim @ ValueInfo(_, _) => prim.value
        case Address(addr) => addr
        case Variable(value, _) =>
          value.value
        case AddressInfo(addy, theType) =>
          context.readVal(addy, TypeHelper.resolve(theType)).value
        case int: Int => int
        case float: Float => float
        case double: Double => double
        case short: Short => short
        case char: Character => char
        case boolean: Boolean => boolean
        case long: Long => long
      }
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
  
  def getPointedType(theType: IType): IType = {
    theType match {
      case ptr: IPointerType => ptr.getType
      case array: IArrayType => array.getType
    }
  }
}