package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.{IArrayType, IType}

import scala.c.engine.*

case class Structure(bytes: Array[Byte], aType: IType) {
	val theType = TypeHelper.stripSyntheticTypeInfo(aType)
	val sizeof = bytes.length

	override def toString = {
		"Structure(size: " + sizeof + ", " + theType.getClass.getSimpleName + ")"
	}
}
