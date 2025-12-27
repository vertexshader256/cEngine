package scala.c.engine

import org.eclipse.cdt.core.dom.ast.{IArrayType, IBasicType, IFunctionType, IPointerType, IQualifierType, IType, ITypedef}
import org.eclipse.cdt.internal.core.dom.parser.c.{CEnumeration, CStructure}

object TypeHelper2 {
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
