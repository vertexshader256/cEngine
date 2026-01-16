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

class State(val sources: List[IASTTranslationUnit], val pointerSize: NumBits) {

	val Stack = Memory(100000)
	val dataSegment = Memory(2000)

	private var heapInsertIndex = 20000

	private val functionContexts = mutable.Stack[FunctionScope]()

	def context: FunctionScope = functionContexts.head

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

	// ************************************************* //
	//                  Constructor                      //
	// ************************************************* //
	
	Functions.scalaFunctions.foreach(addScalaFunctionDef)

	val main: Function = new Function("main", true) {
		def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = None
	}

	pushScope(new FunctionScope(main, null, null) {})
	
	private def pushScope(scope: FunctionScope): Unit = {
		functionContexts.push(scope)
	}

	def getFunctionScope: FunctionScope = {
		functionContexts.collect { case fcnScope: FunctionScope => fcnScope }.head
	}

	def parseGlobals(tUnits: List[IASTNode]): Unit = {
		val program = new FunctionScope(main, null, null) {}
		pushScope(program)
		program.init(tUnits, this, false)

		context.run(this) // parse globals

		context.setAddress(0)
	}

	private def popFunctionContext: FunctionScope = {
		val frame = functionContexts.pop()
		Stack.insertIndex = frame.startingStackAddr
		frame
	}

	def hasFunction(name: String): Boolean = functionList.exists { fcn => fcn.name == name }

	def getFunctionByIndex(index: Int): Function = functionList.find { fcn => fcn.index == index }.get

	def addMain(sources: List[IASTTranslationUnit]): Unit = {
		sources.foreach { tUnit =>
			tUnit.getChildren.collect { case x: IASTFunctionDefinition => x }
				.filter(fcn => fcn.getDeclSpecifier.getStorageClass != IASTDeclSpecifier.sc_extern)
				.foreach { fcnDef =>
					addFunctionDef(fcnDef, fcnDef.getDeclarator.getName.toString == "main")
				}
		}
	}

	private def addScalaFunctionDef(fcn: Function) = {
		val count = functionPointers.size
		fcn.index = count

		functionList += fcn

		val fcnType = CFunctionType(CBasicType(IBasicType.Kind.eVoid, 0), null)
		val newVar = Variable(new CASTName(fcn.name.toCharArray), State.this, fcnType)
		Stack.writeToMemory(count, newVar.address, fcnType)

		functionPointers += fcn.name -> newVar
	}

	private def addStaticFunctionVars(node: IASTNode): List[Variable] = {
		node match {
			case decl: IASTDeclarator =>
				val nameBinding = decl.getName.resolveBinding()

				nameBinding match {
					case vari: IVariable =>
						if (vari.isStatic) {
							val theType = TypeHelper.stripSyntheticTypeInfo(nameBinding.asInstanceOf[IVariable].getType)
							val variable = Variable(decl.getName, this, vari.getType)
							
							if decl.getInitializer != null then
								val initVals = Declarator.getValuesForStaticVar(decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause, theType)(using this)
								Declarator.assign(variable, initVals, null, op_assign)(using this)

							variable.isInitialized = true

							List(variable)
						} else {
							List()
						}
					case _ => List()
				}
			case x => x.getChildren.toList.flatMap { x => addStaticFunctionVars(x) }
		}
	}

	private def addFunctionDef(fcnDef: IASTFunctionDefinition, isMain: Boolean) = {
		val name = fcnDef.getDeclarator.getName
		val count = functionPointers.size

		val fcnType = fcnDef.getDeclarator.getName.resolveBinding().asInstanceOf[IFunction].getType

		val newFcn = new Function(name.toString, true) {
			index = count
			node = fcnDef

			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				None
			}
		}

		newFcn.staticVars ++= addStaticFunctionVars(fcnDef)

		functionList += newFcn

		if (!isMain) {
			val newVar = Variable(name, State.this, fcnType)
			Stack.writeToMemory(count, newVar.address, fcnType)

			functionPointers += name.toString -> newVar
		}
	}

	def callFunctionFromScala(name: String, args: Array[RValue]): Seq[IASTNode] = {
		functionList.find(_.name == name).foreach { fcn =>
			// this is a function simulated in scala
			fcn.run(args.reverse, this).foreach(context.pushOntoStack)
		}

		Seq()
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
					writeDataBlock(struct.bytes, newVar.address)
				case _ =>
					val resolvedArg = TypeHelper.toRValue(arg)(using this)
					val newVar = context.addVariable(param.getName, param.getType)
					val casted = TypeHelper.cast(resolvedArg.value, newVar.theType).value
					Stack.writeToMemory(casted, newVar.address, newVar.theType)
			}
		}
	}

	private def prepareFunctionStackFrame(scope: Option[FunctionScope], function: Function, call: IASTFunctionCallExpression): FunctionScope = {
		val newScope = scope.getOrElse:
			val expressionType = call.getExpressionType
			FunctionScope(function, functionContexts.headOption.orNull, expressionType)

		newScope.init(List(function.node), this, scope.isEmpty)

		val args: List[ValueType] = call.getArguments.map { x => Expressions.evaluate(x)(using this).head }.toList

		args.foreach { argument =>
			if (argument.theType.isInstanceOf[CStructure]) {
				newScope.pushOntoStack(argument)
			} else {
				val resolved = TypeHelper.toRValue(argument)(using this)

				// printf assumes all floating point numbers are doubles
				val promoted = resolved.theType match
					case basic: IBasicType if basic.getKind == IBasicType.Kind.eFloat => RValue(resolved.value, TypeHelper.doubleType)
					case _ => resolved

				newScope.pushOntoStack(promoted)
			}
		}

		newScope.pushOntoStack(RValue(args.size, TypeHelper.unsignedIntType))
		newScope
	}

	def callTheFunction(name: String, call: IASTFunctionCallExpression, scope: Option[FunctionScope], isApi: Boolean = false): Option[ValueType] = {
		functionList.find(_.name == name).flatMap { function =>

			if (!function.isNative) {
				// this is a function simulated in scala

				val stackPos = Stack.insertIndex
				val args = call.getArguments.map { x => Expressions.evaluate(x)(using this) }

				val resolvedArgs: Array[RValue] = args.flatten.map(TypeHelper.toRValue(_)(using this))

				val returnVal = function.run(resolvedArgs.reverse, this)
				Stack.insertIndex = stackPos // pop the stack

				returnVal.map:
					case file @ FileRValue(_) => file
					case rValue => RValue(rValue.value, TypeHelper.unsignedIntType)
			} else {
				if (function.name == "main" && isApi) {
					scope.get.init(List(function.node), this, scope.isEmpty)
					functionContexts.clear()
					functionContexts.push(scope.get)
					context.run(this)
					None
				} else {

					val newScope = prepareFunctionStackFrame(scope, function, call)

					functionContexts.push(newScope)

					newScope.run(this)

					val completedFrame = popFunctionContext

					completedFrame.getReturnValue.map {
						case structure @ LValue(_, structType: CStructure) =>
							val structBytes = structure.toByteArray
							val newAddr = allocateSpace(structBytes.length)
							writeDataBlock(structBytes, newAddr)
							Structure(structBytes, structType)
						case retVal => retVal
					}.orElse {
						None
					}
				}
			}
		}
	}

	def allocateDataSegmentSpace(numBytes: Int): Int = {
		dataSegment.allocate(numBytes)
	}

	def allocateSpace(numBytes: Int): Int = {
		Stack.allocate(numBytes)
	}

	def allocateHeapSpace(numBytes: Int): Int = {
		val result = heapInsertIndex
		heapInsertIndex += Math.max(0, numBytes)
		result
	}

	def copy(dst: Int, src: Int, numBytes: Int): Unit = {
		Stack.copy(dst, src, numBytes)
	}

	def set(dst: Int, value: Byte, numBytes: Int): Unit = {
		Stack.set(dst, value, numBytes)
	}

	def writeDataBlock(array: Array[Byte], startingAddress: Int): Unit = {
		Stack.writeDataBlock(array, startingAddress)
	}

	def readDataBlock(startingAddress: Int, length: Int): Array[Byte] = {
		Stack.readDataBlock(startingAddress, length)
	}

	def readPtrVal(address: Int): Int = {
		Stack.readPtrVal(address)
	}

	private def stripQuotes(str: String): String = {
		str.tail.reverse.tail.reverse
	}

	def getString(str: String): RValue = {
		val theStr = stripQuotes(str)

		val withNull = (theStr.toCharArray :+ 0.toChar).map(_.toByte) // terminating null char
		val strAddr = allocateSpace(withNull.length)

		writeDataBlock(withNull, strAddr)
		RValue(strAddr, pointerType)
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

	def writeDataBlock(array: List[RValue], startingAddress: Int): Unit = {
		var address = startingAddress

		array.foreach:
			case RValue(newVal, theType) =>
				Stack.writeToMemory(newVal, address, theType)
				address += TypeHelper.sizeof(theType)(using this)
	}
}
