package scala.c.engine

import org.eclipse.cdt.internal.core.dom.parser.c._
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind._
import org.eclipse.cdt.core.dom.ast._

import scala.c.engine.ast.Expressions

object TypeHelper {

  val one = new RValue(1, new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_UNSIGNED)) {}
  val zero = new RValue(0, new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_UNSIGNED)) {}
  val negativeOne = new RValue(-1, new CBasicType(IBasicType.Kind.eInt, 0)) {}
  
  // 8 bytes
  val qword = new CBasicType(IBasicType.Kind.eInt , IBasicType.IS_LONG_LONG)

  def castSign(theType: IType, newVal: AnyVal): RValue = {
    val casted: AnyVal = theType match {
      case basic: IBasicType =>
        if (basic.isUnsigned) {
          newVal match {
            case long: Long => long & 0x00000000FFFFFFFFL
            case int: Int => int & 0xFFFFFFFFL
            case short: Short => short & 0xFFFF
            case byte: Byte => byte & 0xFF
          }
        } else {
          newVal
        }
      case _ => newVal
    }

    new RValue(casted, theType) {}
  }

  // Kind of hacky; this will do whatever it needs to match gcc.  casts 'AnyVal' to 'ValueInfo'
  def cast(theType: IType, theVal: AnyVal): RValue = {
    val casted: AnyVal = theType match {
      case typedef: CTypedef => theVal
      case qual: IQualifierType => theVal
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
              case long: Long => long.toChar.toByte
              case char: char => char
              case char: Char => char.toByte
            }
         case `eInt` if basic.isLongLong =>
           newVal match {
             case int: Int => int.toLong
             case long: Long => long
             case double: Double => double.toLong
             case float: Float => float.toLong
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
    
    new RValue(casted, theType) {}
  }

  def getType(idExpr: IASTTypeId) = idExpr.getDeclSpecifier match {
    case simple: IASTSimpleDeclSpecifier =>
      import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclSpecifier._

      var config = 0

      if (simple.isLongLong) {
        config |= IBasicType.IS_LONG_LONG
      }

      if (simple.isLong) {
        config |= IBasicType.IS_LONG
      }

      if (simple.isShort) {
        config |= IBasicType.IS_SHORT
      }

      if (simple.isUnsigned) {
        config |= IBasicType.IS_UNSIGNED
      } else {
        config |= IBasicType.IS_SIGNED
      }

      var result: IType = simple.getType match {
        case `t_unspecified` => new CBasicType(IBasicType.Kind.eInt, config)
        case `t_int`    => new CBasicType(IBasicType.Kind.eInt, config)
        case `t_float`  => new CBasicType(IBasicType.Kind.eFloat, config)
        case `t_double` => new CBasicType(IBasicType.Kind.eDouble, config)
        case `t_char`   => new CBasicType(IBasicType.Kind.eChar, config)
        case `t_void`   => new CBasicType(IBasicType.Kind.eVoid, config)
      }

      for (ptr <- idExpr.getAbstractDeclarator.getPointerOperators) {
        result = new CPointerType(result, 0)
      }

      TypeInfo(result)

    case typespec: CASTTypedefNameSpecifier =>
      TypeInfo(typespec.getName.resolveBinding().asInstanceOf[IType])
    case elab: CASTElaboratedTypeSpecifier =>
      TypeInfo(elab.getName.resolveBinding().asInstanceOf[CStructure])
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
  
  // resolves 'ValueType' to 'RValue'
  def resolve(any: ValueType)(implicit state: State): RValue = {
    any match {
      case info @ LValue(_, _) => info.value
      case rValue @ RValue(_, _) => rValue
      case StringLiteral(str) => state.createStringVariable(str, false)
    }
  }

  def isPointer(value: ValueType): Boolean = {
    isPointer(value.theType) || value.isInstanceOf[Address]
  }

  def isPointer(theType: IType): Boolean = {
    theType.isInstanceOf[IPointerType] || theType.isInstanceOf[IArrayType]
  }

  def not(theVal: Any): AnyVal = theVal match {
    case info @ LValue(_, _) => not(info.value)
    case RValue(theVal, _) => not(theVal)
    case int: Int => if (int == 0) 1 else 0
    case long: Long => if (long == 0) 1 else 0
    case bool: Boolean => !bool
    case char: char => if (char == 0) 1 else 0
  }

  def resolveBasic(theType: IType)(implicit state: State): IBasicType = theType match {
    case basicType: IBasicType    => basicType
    case typedef: ITypedef        => resolveBasic(typedef.getType)
    case ptrType: IPointerType    => resolveBasic(ptrType.getType)
    case arrayType: IArrayType    => resolveBasic(arrayType.getType)
    case qualType: IQualifierType => resolveBasic(qualType.getType)
    case fcn: IFunctionType       => state.pointerType
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

  def getPointerType(theType: IType): IType = theType match {
    case typedef: ITypedef        => getPointerType(typedef.getType)
    case ptrType: IPointerType    => ptrType.getType
    case arrayType: IArrayType    => arrayType.getType
    case qualType: IQualifierType => getPointerType(qualType.getType)
  }
  
  def resolveBoolean(theVal: Any): Boolean = theVal match {
      case x: Boolean => x
      case int: int => int != 0
      case short: short => short != 0
      case char: char => char != 0
      case long: Long => long != 0
      case float: Float => float != 0.0
      case double: Double => double != 0.0
      case RValue(value, _) => resolveBoolean(value)
      case info @ LValue(_, _) => resolveBoolean(info.value)
  }

  def offsetof(struct: CStructure, memberName: String, state: State): Int = {
    struct.getFields.takeWhile{field => field.getName != memberName}.map{x => TypeHelper.sizeInBits(x)(state) / 8}.sum
  }

  def offsetof(structType: CStructure, baseAddress: Int, fieldName: String, state: State) = {
    var resultAddress: Field = null
    var offsetInBits: Int = 0

    structType.getKey match {
      case ICompositeType.k_struct =>
        structType.getFields.foreach{field =>
          if (field.getName == fieldName) {
            // can assume names are unique
            resultAddress = Field(state, baseAddress + offsetInBits / 8, offsetInBits % 8, field.getType, TypeHelper.sizeInBits(field)(state))
          } else {
            offsetInBits += TypeHelper.sizeInBits(field)(state)
          }
        }
      case ICompositeType.k_union =>
        // TODO: Unions and bit fields dont work
        structType.getFields.find{field => field.getName == fieldName}.foreach { field =>
          resultAddress = Field(state, baseAddress, 0, field.getType, TypeHelper.sizeInBits(field)(state))
        }
    }
    resultAddress
  }

  def resolveStruct(theType: IType): CStructure = theType match {
    case qual: CQualifierType =>
      resolveStruct(qual.getType)
    case typedef: CTypedef =>
      resolveStruct(typedef.getType)
    case struct: CStructure => struct
    case ptr: IPointerType =>
      resolveStruct(ptr.getType)
  }

  def sizeof(theType: IType)(implicit state: State): Int = theType match {
    case fcn: IFunctionType => sizeof(state.pointerType)
    case ptr: IPointerType => sizeof(state.pointerType)
    case struct: CStructure =>
      val numBits = struct.getKey match {
        case ICompositeType.k_struct =>
          struct.getFields.map { field =>
            sizeInBits(field)
          }.sum
        case ICompositeType.k_union =>
          var maxSize = 0
          struct.getFields.foreach { field =>
            val size = sizeInBits(field)
            if (size > maxSize) {
              maxSize = size
            }
          }
          maxSize
      }

      Math.ceil(Math.max(numBits, 32) / 32.0).toInt * 4
    case array: IArrayType =>
      sizeof(array.getType)
    case typedef: CTypedef =>
      if (typedef.getType.isInstanceOf[IArrayType]) {
        sizeof(typedef.getType) * typedef.getType.asInstanceOf[IArrayType].getSize.numericalValue().toInt
      } else {
        sizeof(typedef.getType)
      }
    case qual: IQualifierType =>
      sizeof(qual.getType)
    case basic: IBasicType =>
      basic.getKind match {
        case `eInt` if basic.isLongLong => 8
        case `eInt` if basic.isLong     => 4
        case `eInt` if basic.isShort    => 2
        case `eInt`                     => 4
        case `eFloat`                   => 4
        case `eDouble`                  => 8
        case `eChar`                    => 1
        case `eVoid`                    => 1
        case `eBoolean`                 => 4
      }
  }

  def sizeInBits(field: IField)(implicit state: State): Int = {
    val parent = field.asInstanceOf[CField].getDefinition.getParent
    parent match {
      case field: CASTFieldDeclarator => Expressions.evaluate(field.getBitFieldSize).get.asInstanceOf[RValue].value.asInstanceOf[Int]
      case _ => field.getType match {
        case array: IArrayType =>
          sizeof(array.getType) * array.getSize.numericalValue().toInt * 8
        case x =>
          sizeof(x) * 8
      }
    }
  }
}