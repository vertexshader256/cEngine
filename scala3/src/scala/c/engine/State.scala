package scala.c.engine

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.op_assign
import org.eclipse.cdt.internal.core.dom.parser.c.*

import scala.c.engine.Instructions.*
import scala.c.engine.ast.{Declarator, Expressions}
import scala.c.engine.cFunctions.*
import scala.c.engine.models.*
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class State(val sources: List[IASTTranslationUnit], val pointerSize: NumBits) extends CodeRunner {

	val stack = Memory(stackSize = 100000, dataSize = 10000, heapSize = 50000)

	var varArgStartingAddr = List[Int]()
	val scalaFunctions = ListBuffer[Function]()

	val functionList = ListBuffer[Function]()
	val functionPointers = scala.collection.mutable.LinkedHashMap[String, Variable]()
	val stdout = ListBuffer[Char]()

	var breakLabelStack = List[Label]()
	var continueLabelStack = List[Label]()

	val structs: Seq[CStructure] = sources.flatMap { src =>
		src.getDeclarations.collect { case simp: CASTSimpleDeclaration => simp.getDeclSpecifier }
			.collect { case comp: CASTCompositeTypeSpecifier => comp }
			.map { x => x.getName.resolveBinding().asInstanceOf[CStructure] }
	}

	val pointerType: CBasicType = pointerSize match
		case NumBits.ThirtyTwoBits => TypeHelper.intType
		case NumBits.SixtyFourBits => CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG_LONG)

	val addressSize: Int = TypeHelper.sizeof(pointerType)(using this)

	def hasFunction(name: String): Boolean = functionList.exists { fcn => fcn.name == name }

	def getFunctionByIndex(index: Int): Function = functionList.find { fcn => fcn.index == index }.get

	private def addScalaFunctionDef(fcn: Function) = {
		val count = functionPointers.size
		fcn.index = count

		functionList += fcn

		val fcnType = CFunctionType(CBasicType(IBasicType.Kind.eVoid, 0), null)
		val newVar = Variable(new CASTName(fcn.name.toCharArray), State.this, fcnType)
		stack.writeToMemory(count, newVar.address, fcnType)

		functionPointers += fcn.name -> newVar
	}

	def writeFunctionStackFrame(fcnDec: IASTFunctionDeclarator): Unit = {
		val numArgs = context.popStack.asInstanceOf[RValue].value.asInstanceOf[Integer] // placed on the stack by prepareFunctionStackFrame()
		val args = (0 until numArgs).map { _ => context.popStack }.reverse

		val binding = fcnDec.getName.resolveBinding()
		val fcn = binding.asInstanceOf[CFunction]
		val paramDecls = fcn.getParameters.toList
		val zipped = args.zip(paramDecls)

		zipped.foreach { (arg, param) =>
			arg match {
				case variable: Variable if variable.aType.isInstanceOf[CStructure] =>
					val copy = Structures.copyStructure(variable.aType.asInstanceOf[CStructure], variable.address, CASTName(param.getName.toCharArray), this)
					context.addVariable(copy)
				case struct: Structure =>
					val newVar = context.addVariable(param.getName, param.getType)
					writeDataBlock(newVar.address, struct.bytes)
				case _ =>
					val resolvedArg = TypeHelper.toRValue(arg)(using this)
					val newVar = context.addVariable(param.getName, param.getType)
					val casted = TypeHelper.cast(resolvedArg.value, newVar.theType).value
					stack.writeToMemory(casted, newVar.address, newVar.theType)
			}
		}
	}

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

	def writeValues(address: Address, values: List[RValue]): Unit = {
		var location = address.location

		values.foreach:
			case RValue(newVal, theType) =>
				stack.writeToMemory(newVal, Address(location), theType)
				location += TypeHelper.sizeof(theType)(using this)
	}

	// ************************************************* //
	//                  Constructor                      //
	// ************************************************* //

	Stdio.addFunctions(scalaFunctions)(using this)
	Mathh.addFunctions(scalaFunctions)(using this)
	Stdlibh.addFunctions(scalaFunctions)(using this)
	Stringh.addFunctions(scalaFunctions)(using this)
	Stdargh.addFunctions(scalaFunctions)

	scalaFunctions.foreach(addScalaFunctionDef)
}
