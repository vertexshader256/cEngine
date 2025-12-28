package scala.c.engine

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

object TypeHelper2 {

	val intType = new CBasicType(IBasicType.Kind.eInt, 0)
	val charType = new CBasicType(IBasicType.Kind.eChar, 0)
	val unsignedIntType = new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_UNSIGNED)
	val doubleType = new CBasicType(IBasicType.Kind.eDouble, 0)
	val floatType = new CBasicType(IBasicType.Kind.eFloat, 0)

	val one = RValue(1, unsignedIntType)
	val zero = RValue(0, unsignedIntType)
	val negativeOne = RValue(-1, intType)

	def getLong(lit: String) =
		RValue(lit.toLong, new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG))

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
		}

		RValue(casted, theType)
	}

	def stripSyntheticTypeInfo(theType: IType): IType = theType match {
		case enumer: CEnumeration => enumer
		case struct: CStructure => struct
		case basicType: IBasicType => basicType
		case typedef: ITypedef => stripSyntheticTypeInfo(typedef.getType)
		case ptrType: IPointerType => ptrType
		case arrayType: IArrayType => arrayType
		case qualType: IQualifierType => stripSyntheticTypeInfo(qualType.getType)
		case fcn: IFunctionType => fcn
	}
}
