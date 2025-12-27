package scala.c.engine

import org.eclipse.cdt.core.dom.ast.{IArrayType, IBasicType, IFunctionType, IPointerType, IQualifierType, IType, ITypedef}
import org.eclipse.cdt.internal.core.dom.parser.c.{CEnumeration, CStructure}

object TypeHelper2 {

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
