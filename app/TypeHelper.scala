package app.astViewer

import org.eclipse.cdt.internal.core.dom.parser.c._
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._
import org.eclipse.cdt.core.dom.ast._

object TypeHelper {
  
  // 32-bit pointers
  val pointerType = new CBasicType(IBasicType.Kind.eInt , 0)
  
  // 8 bytes
  val qword = new CBasicType(IBasicType.Kind.eInt , IBasicType.IS_LONG_LONG)

  def castSign(theType: IType, newVal: AnyVal): ValueInfo = {
    val casted: AnyVal = theType match {
      case basic: IBasicType =>
        if (basic.isUnsigned) {
          newVal match {
            case long: Long => long & 0x00000000FFFFFFFFL
            case int: Int => int & 0xFFFFFFFF
            case short: Short => short & 0xFFFF
            case byte: Byte => byte & 0xFF
          }
        } else {
          newVal
        }
      case _ => newVal
    }

    ValueInfo(casted, theType)
  }

  // Kind of hacky; this will do whatever it needs to match gcc.  casts 'AnyVal' to 'ValueInfo'
  def cast(theType: IType, theVal: AnyVal): ValueInfo = {
    val casted: AnyVal = theType match {
      case typedef: CTypedef => cast(typedef.getType, theVal).value
      case qual: IQualifierType => cast(qual.getType, theVal).value
      case fcn: IFunctionType => theVal
      case struct: CStructure =>  theVal
      case ptr: IPointerType => theVal
      case array: IArrayType => theVal
      case basic: IBasicType =>

       val newVal = if (basic.isUnsigned) {
         castSign(theType, theVal).value
       } else {
         theVal
       }

       basic.getKind match {
          case `eChar`    => 
            newVal match {
              case int: Int => int.toChar.toByte
              case char: Character => char
              case char: Char => char.toByte
            }
         case `eInt` if basic.isLong =>
            newVal match {
              case int: Int => int.toLong
              case long: Long => long
              case double: Double => double.toLong
              case float: Float => float.toLong
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
              case long: Long => long.toFloat
            }  
         case `eDouble`  =>
            newVal match {
              case int: Int => int.toDouble
              case long: Long => long.toDouble
              case double: Double => double
              case float: Float => float.toDouble
            } 
          case `eBoolean` =>
            if (TypeHelper.resolveBoolean(newVal)) 1 else 0
          case `eVoid` =>
            newVal
        }
      }
    
    ValueInfo(casted, theType)
  }

  def getType(value: AnyVal): IBasicType = {
    var config = 0
              
    if (value.isInstanceOf[Long]) {
      config |= IBasicType.IS_LONG
    }
    
    if (value.isInstanceOf[Short]) {
      config |= IBasicType.IS_SHORT
    }
    
    value match {
      case StringAddress(_)    => pointerType
      case bool: Boolean => new CBasicType(IBasicType.Kind.eBoolean, config)
      case int: Int      => new CBasicType(IBasicType.Kind.eInt, config)
      case long: Long    => new CBasicType(IBasicType.Kind.eInt, config)
      case float: Float  => new CBasicType(IBasicType.Kind.eFloat, config)
      case doub: Double  => new CBasicType(IBasicType.Kind.eDouble, config)           
      case char: Char    => new CBasicType(IBasicType.Kind.eChar, config)
      case short: Short  => new CBasicType(IBasicType.Kind.eInt, config)
      case char: Character => new CBasicType(IBasicType.Kind.eChar, config)
    }
  }
  
  def isSigned(theType: IBasicType) = {
    theType.isSigned || (!theType.isSigned && !theType.isUnsigned)
  }
  
  // resolves 'Any' to 'ValueInfo'
  def resolve(any: Any)(implicit state: State): ValueInfo = {
    any match {
      case StringAddress(x) => ValueInfo(x, TypeHelper.pointerType)
      case info @ AddressInfo(_, _) => info.value
      case value @ ValueInfo(theVal, _) => value
      case int: Int => ValueInfo(int, null)
      case short: Short => ValueInfo(short, null)
      case float: Float => ValueInfo(float, null)
      case double: Double => ValueInfo(double, null)
      case long: Long => ValueInfo(long, null)
      case bool: Boolean => ValueInfo(if (bool) 1 else 0, null)
      case byte: Byte => ValueInfo(byte, null)
    }
  }

  def getBaseType(theType: IType): IType = theType match {
    case struct: CStructure       => struct
    case basicType: IBasicType    => basicType
    case typedef: ITypedef        => getBaseType(typedef.getType)
    case ptrType: IPointerType    => getBaseType(ptrType.getType)
    case arrayType: IArrayType    => getBaseType(arrayType.getType)
    case qualType: IQualifierType => getBaseType(qualType.getType)
    case fcn: IFunctionType       => fcn
  }
  
  // Some standard cases of needing to know what a type resolves to
  def resolve(theType: IType): IBasicType = theType match {
    case struct: CStructure       => TypeHelper.pointerType
    case basicType: IBasicType    => basicType
    case typedef: ITypedef        => resolve(typedef.getType)
    case ptrType: IPointerType    => resolve(ptrType.getType)
    case arrayType: IArrayType    => resolve(arrayType.getType)
    case qualType: IQualifierType => resolve(qualType.getType)
    case fcn: IFunctionType       => TypeHelper.pointerType
  }
  
  def stripSyntheticTypeInfo(theType: IType): IType = theType match {
    case struct: CStructure       => struct
    case basicType: IBasicType    => basicType
    case typedef: ITypedef        => resolve(typedef.getType)
    case ptrType: IPointerType    => ptrType
    case arrayType: IArrayType    => arrayType
    case qualType: IQualifierType => resolve(qualType.getType)
    case fcn: IFunctionType       => fcn
  }
  
  def resolveBoolean(theVal: Any): Boolean = theVal match {
      case x: Boolean => x
      case int: Int => int > 0
      case long: Long => long > 0
      case short: Short => short > 0
      case char: Character => char > 0
      case ValueInfo(value, _) => resolveBoolean(value)
      case info @ AddressInfo(_, _) => resolveBoolean(info.value)
  }

  def sizeof(theType: IType): Int = theType match {
    case fcn: IFunctionType => 4
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
        //case `eInt` if basic.isLongLong => 8
        case `eInt` if basic.isLong     => 8 // BUG: SHOULD BE 4
        case `eInt`                     => 4
        case `eFloat`                   => 4
        case `eChar16`                  => 2
        case `eDouble`                  => 8
        case `eChar`                    => 1
        case `eChar32`                  => 4
        case `eVoid`                    => 4
        case `eBoolean`                 => 4
      }
  }
  
  def isPointer(theType: IType): Boolean = theType match {
    case array: IArrayType => true
    case ptr: IPointerType => true
    case typedef: ITypedef => isPointer(typedef.getType)
    case qual: IQualifierType => isPointer(qual.getType)
    case struct: CStructure => false
    case basic: IBasicType => false
    case null => false
  }

  def getPointedType(theType: IType): IType = {
    theType match {
      case ptr: IPointerType => ptr.getType
      case array: IArrayType => array.getType
      case typedef: ITypedef => getPointedType(typedef.getType)
      case qual: IQualifierType => getPointedType(qual.getType)
    }
  }
}