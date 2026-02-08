package scala.c.engine

import scala.c.engine.models.Address
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.op_assign
import org.eclipse.cdt.internal.core.dom.parser.c.*

import scala.c.engine.Instructions.*
import scala.c.engine.ast.{Declarator, Expressions}
import scala.c.engine.cFunctions.*
import scala.c.engine.models.*
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait CEngineData {
	this: State =>

	val pointerType: CBasicType = pointerSize match
		case NumBits.ThirtyTwoBits => TypeHelper.intType
		case NumBits.SixtyFourBits => CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG_LONG)

	val addressSize: Int = TypeHelper.sizeof(pointerType)(using this)

	def allocateDataSegmentSpace(numBytes: Int): Address = {
		stack.allocateData(numBytes)
	}

	def allocateStack(numBytes: Int): Address = {
		stack.allocate(numBytes)
	}

	def allocateHeapSpace(numBytes: Int): Address = {
		stack.allocateHeapSpace(numBytes)
	}

	def copy(dst: Address, src: Address, numBytes: Int): Unit = {
		val data = stack.readDataBlock(src, numBytes)
		stack.writeDataBlock(data, dst)
	}

	def set(address: Address, value: Byte, numBytes: Int): Unit = {
		stack.set(address, value, numBytes)
	}

	def writeDataBlock(address: Address, array: Array[Byte]): Unit = {
		stack.writeDataBlock(array, address)
	}

	def writeValues(address: Address, values: List[RValue]): Unit = {
		var location = address.location

		values.foreach:
			case RValue(newVal, theType) =>
				stack.writeToMemory(newVal, Address(location), theType)
				location += TypeHelper.sizeof(theType)(using this)
	}

	def readDataBlock(address: Address, length: Int): Array[Byte] = {
		stack.readDataBlock(address, length)
	}

	def readPtrVal(address: Address): Int = {
		stack.readPtrVal(address)
	}

	private def stripQuotes(str: String): String = {
		str.tail.reverse.tail.reverse
	}

	def allocateString(str: String, isStatic: Boolean = false): RValue = {
		val theStr = stripQuotes(str)

		val withNull = (theStr.toCharArray :+ 0.toChar).map(_.toByte) // terminating null char

		val strAddr = if isStatic then
			allocateDataSegmentSpace(withNull.length)
		else
			allocateStack(withNull.length)

		writeDataBlock(strAddr, withNull)
		RValue(strAddr.location, pointerType)
	}

	def createStringArrayVariable(varName: IASTName, str: String, theType: IType): Variable = {
		val theStr = stripQuotes(str)
		val translateLineFeed = theStr.replace("\\n", 10.asInstanceOf[Char].toString)
		val withNull = (translateLineFeed.toCharArray :+ 0.toChar)
			.map { char => RValue(char.toByte, TypeHelper.charType) }.toList // terminating null char

		val inferredArrayType = CArrayType(theType)
		inferredArrayType.setModifier(CASTArrayModifier(CASTLiteralExpression(IASTLiteralExpression.lk_integer_constant, str.length.toString.toCharArray)))

		val theArrayPtr = context.addVariable(varName, inferredArrayType, withNull)
		theArrayPtr
	}
}
