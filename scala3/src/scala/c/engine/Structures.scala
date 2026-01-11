package scala.c.engine

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclSpecifier._
import scala.c.engine.ast.Expressions
import IBasicType.Kind.*
import IBasicType.*
import scala.c.engine.models.*

object Structures {

	def isStructure(theType: IType): Boolean = theType match {
		case struct: CStructure => true
		case basicType: IBasicType => false
		case typedef: ITypedef => isStructure(typedef.getType)
		case arrayType: IArrayType => isStructure(arrayType.getType)
	}

	def copyStructure(src: Variable, state: State): Variable = {
		val structType = src.theType.asInstanceOf[CStructure]
		val newAddress = state.Stack.insertIndex
		val resultCopy = Variable(src.name, state, src.theType) // space is allocated now

		println("Copy size: " + resultCopy.sizeof)
		println("Source structure address: " + src.address)
		println("New structure address: " + resultCopy.address)
		structType.getFields.foreach: field =>
			val srcField = offsetof(structType, src.address, field.getName, state)
			val srcFieldValue = srcField.rValue
			val dstField = offsetof(structType, newAddress, field.getName, state)
			println(s"copying from address (${srcField.address}) to address (${dstField.address})")
			println(s"copying value (${srcFieldValue.value})")
			state.Stack.writeToMemory(srcFieldValue.value, dstField.address, srcField.theType)

		resultCopy
	}

	def offsetof(struct: CStructure, memberName: String, state: State): Int = {
		val largestField = struct.getFields.filter { f => f.getType.isInstanceOf[CBasicType] }.map { x => sizeInBits(x)(using state) / 8 }.sorted.maxOption.getOrElse(0)
		val fields = struct.getFields.takeWhile { field => field.getName != memberName }.map { x => sizeInBits(x)(using state) / 8 }
		val paddedFields = fields.map: f =>
			if f < largestField then largestField else f // gcc adds padding
		paddedFields.sum
	}

	def offsetof(structType: CStructure, baseAddress: Int, fieldName: String, state: State): Field = {
		var resultAddress: Field = null
		var offsetInBits: Int = 0

		structType.getKey match {
			case ICompositeType.k_struct =>
				structType.getFields.foreach: field =>
					if field.getName == fieldName then
						// can assume names are unique
						resultAddress = Field(state, baseAddress + offsetInBits / 8, offsetInBits % 8, field.getType, sizeInBits(field)(using state))
					else
						offsetInBits += sizeInBits(field)(using state)
			case ICompositeType.k_union =>
				// TODO: Unions and bit fields dont work
				structType.getFields.find { field => field.getName == fieldName }.foreach: field =>
					resultAddress = Field(state, baseAddress, 0, field.getType, sizeInBits(field)(using state))
		}

		resultAddress
	}

	def sizeInBits(field: IField)(implicit state: State): Int = {
		val parent = field.asInstanceOf[CField].getDefinition.getParent
		parent match
			case field: CASTFieldDeclarator => Expressions.evaluate(field.getBitFieldSize).get.asInstanceOf[RValue].value.asInstanceOf[Int]
			case _ => field.getType match
				case array: IArrayType =>
					TypeHelper.sizeof(array.getType) * array.getSize.numericalValue().toInt * 8
				case x =>
					TypeHelper.sizeof(x) * 8
	}

	def resolveStruct(theType: IType): CStructure = theType match {
		case qual: CQualifierType => resolveStruct(qual.getType)
		case typedef: CTypedef => resolveStruct(typedef.getType)
		case struct: CStructure => struct
		case ptr: IPointerType => resolveStruct(ptr.getType)
	}
}
