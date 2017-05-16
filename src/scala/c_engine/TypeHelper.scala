package scala.c_engine

import org.eclipse.cdt.internal.core.dom.parser.c._
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._
import org.eclipse.cdt.core.dom.ast._

object TypeHelper {
  
  // 32-bit pointers
  val pointerType = new CBasicType(IBasicType.Kind.eInt , 0)
  
  // 8 bytes
  val qword = new CBasicType(IBasicType.Kind.eInt , IBasicType.IS_LONG_LONG)

  def castSign(theType: IType, newVal: AnyVal): RValue = {
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

    RValue(casted, theType)
  }

  // Kind of hacky; this will do whatever it needs to match gcc.  casts 'AnyVal' to 'ValueInfo'
  def cast(theType: IType, theVal: AnyVal): RValue = {
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
              case char: char => char
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
              case char: char => char.toInt
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
    
    RValue(casted, theType)
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
      case bool: Boolean => new CBasicType(IBasicType.Kind.eBoolean, config)
      case int: Int      => new CBasicType(IBasicType.Kind.eInt, config)
      case long: Long    => new CBasicType(IBasicType.Kind.eInt, config)
      case float: Float  => new CBasicType(IBasicType.Kind.eFloat, config)
      case doub: Double  => new CBasicType(IBasicType.Kind.eDouble, config)           
      case char: Char    => new CBasicType(IBasicType.Kind.eChar, config)
      case short: Short  => new CBasicType(IBasicType.Kind.eInt, config)
      case char: char => new CBasicType(IBasicType.Kind.eChar, config)
    }
  }
  
  // resolves 'Any' to 'ValueInfo'
  def resolve(any: ValueType)(implicit state: State): RValue = {
    any match {
      case info @ LValue(_, _) => info.value
      case value @ RValue(theVal, _) => value
    }
  }

  // Some standard cases of needing to know what a type resolves to
  def resolve(theType: IType): IBasicType = theType match {
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
    case typedef: ITypedef        => stripSyntheticTypeInfo(typedef.getType)
    case ptrType: IPointerType    => ptrType
    case arrayType: IArrayType    => arrayType
    case qualType: IQualifierType => stripSyntheticTypeInfo(qualType.getType)
    case fcn: IFunctionType       => fcn
  }
  
  def resolveBoolean(theVal: Any): Boolean = theVal match {
      case x: Boolean => x
      case int: int => int != 0
      case short: short => short != 0
      case char: char => char != 0
      case RValue(value, _) => resolveBoolean(value)
      case info @ LValue(_, _) => resolveBoolean(info.value)
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
        case `eInt` if basic.isLong     => 4
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
}