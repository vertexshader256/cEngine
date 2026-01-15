package scala.c.engine.models

import org.eclipse.cdt.core.dom.ast.{IArrayType, IType}

import scala.c.engine.*

case class Structure(bytes: Array[Byte], rawType: IType) extends ValueType {
	val theType = TypeHelper.stripSyntheticTypeInfo(rawType)
	val sizeof = bytes.length

	override def toString = {
		"Structure(size: " + sizeof + ", " + theType.getClass.getSimpleName + ")"
	}
}
