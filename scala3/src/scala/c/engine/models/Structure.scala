package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.{IArrayType, IType}

import scala.c.engine.*

case class Structure(bytes: Array[Byte], aType: IType, sizeof: Int) {
	val theType = TypeHelper.stripSyntheticTypeInfo(aType)

	override def toString = {
		"Structure(size: " + sizeof + ", address: " + address + ", " + theType.getClass.getSimpleName + ")"
	}
}
