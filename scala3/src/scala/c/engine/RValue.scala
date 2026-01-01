package scala.c.engine

import org.eclipse.cdt.core.dom.ast.IType

object RValue {
	def unapply(rvalue: RValue): Option[(cEngVal, IType)] = Some((rvalue.value, rvalue.theType))

	def apply(theValue: cEngVal, aType: IType) =
		new RValue {
			val theType = TypeHelper.stripSyntheticTypeInfo(aType);
			val rawType = aType
			val value = theValue
		}

	def apply(theValue: cEngVal) =
		new RValue {
			val theType: IType = null
			val rawType: IType = null
			val value = theValue;
		}
}

// An RValue is an expression that has a value, a type, and no memory address
abstract class RValue extends ValueType {
	val value: cEngVal
	val theType: IType

	override def toString = {
		"RValue(" + value + ", " + theType + ")"
	}
}
