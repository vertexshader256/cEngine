package app.astViewer

import org.eclipse.cdt.internal.core.dom.parser.c._
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._
import org.eclipse.cdt.core.dom.ast._

object TypeHelper {
  
  // 32-bit pointers
  val pointerType = new CBasicType(IBasicType.Kind.eInt , 0)
  
  // 8 bytes
  val qword = new CBasicType(IBasicType.Kind.eInt , IBasicType.IS_LONG_LONG)
  
  // Kind of hacky; this will do whatever it needs to match gcc.  casts 'AnyVal' to 'ValueInfo'
  def cast(theType: IType, newVal: AnyVal): ValueInfo = {
    val casted: AnyVal = theType match {
      case typedef: CTypedef => cast(typedef.getType, newVal).value
      case qual: IQualifierType => cast(qual.getType, newVal).value
      case fcn: IFunctionType => newVal.asInstanceOf[Int]
      case ptr: IPointerType => newVal match {
        case Address(addy) => addy
        case int: Int => int
      }
      case array: IArrayType => if (newVal.isInstanceOf[Address]) newVal.asInstanceOf[Address] else newVal.asInstanceOf[Int]
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
              case double: Double => double.toLong
            } 
         case `eInt` if basic.isLong =>
            newVal match {
              case int: Int => int.toLong
              case long: Long => long
              case double: Double => double.toLong
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
              case Address(addy) => addy
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
  
  // kind of hacky here, but needed a way to be a bit more strict about casting in regards to memory space
  def downcast(theType: IType, newVal: AnyVal): ValueInfo = {
    val casted: AnyVal = theType match {
      case typedef: CTypedef => cast(typedef.getType, newVal).value
      case qual: IQualifierType => cast(qual.getType, newVal).value
      case ptr: IPointerType => cast(ptr, newVal).value
      case array: IArrayType => cast(array, newVal).value
      case basic: IBasicType =>
       basic.getKind match {
          case `eChar` if basic.isUnsigned    => 
            newVal match {
              case int: Int => (int & 0xFFFFFFFF).toChar.toByte
              case char: Character => char & 0xFF
            }  
         case _ =>
            cast(basic, newVal).value
        }
       case _ =>
            cast(theType, newVal).value
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
      case Address(_)    => pointerType
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
      case lit @ Literal(_) => lit.cast
      case Address(addy) => ValueInfo(addy, TypeHelper.pointerType)
      case Addressable(AddressInfo(addy, theType)) =>
          state.readVal(addy, theType)
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
  
  def resolveBoolean(theVal: Any)(implicit context: State): Boolean = theVal match {
      case x: Boolean => x
      case int: Int => int > 0
      case char: Character => char > 0
      case ValueInfo(value, theType) => resolveBoolean(value)
      case Addressable(AddressInfo(addr, theType)) => resolveBoolean(context.readVal(addr, theType).value)
      case x: Literal => resolveBoolean(x.cast.value)
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