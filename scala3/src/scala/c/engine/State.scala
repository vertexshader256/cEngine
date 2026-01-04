package scala.c.engine

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.op_assign
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import java.util
import scala.annotation.tailrec
import scala.c.engine.Instructions.*
import scala.c.engine.ast.{Declarator, Expressions}
import scala.collection.mutable.ListBuffer

object State {

	private def compile(node: IASTNode)(implicit state: State): List[Any] = {
		node match
			case ifStatement: IASTIfStatement =>
				compileIfStatement(ifStatement)
			case forStatement: IASTForStatement =>
				compileForStatement(forStatement)
			case whileStatement: IASTWhileStatement =>
				compileWhileStatement(whileStatement)
			case doWhileStatement: IASTDoStatement =>
				compileDoWhileStatement(doWhileStatement)
			case switch: IASTSwitchStatement =>
				compileSwitcheStatement(switch)
			case x: IASTCaseStatement =>
				List(CaseLabel(x))
			case x: IASTDefaultStatement =>
				List(DefaultLabel(x))
			case _: IASTContinueStatement =>
				List(JmpLabel(state.continueLabelStack.head))
			case _: IASTBreakStatement =>
				List(JmpLabel(state.breakLabelStack.head))
			case _: IASTElaboratedTypeSpecifier =>
				List()
			case goto: IASTGotoStatement =>
				List(JmpName(goto.getName.toString))
			case fcn: IASTFunctionDefinition =>
				List(fcn)
			case compound: IASTCompoundStatement =>
				compileCompoundStatement(compound)
			case decl: IASTDeclarationStatement =>
				decl.getChildren.toList.flatMap(compile)
			case decl: CASTSimpleDeclaration =>
				List(decl)
			case _: IASTSimpleDeclSpecifier =>
				List()
			case _: CASTTypedefNameSpecifier =>
				List()
			case decl: IASTDeclarator =>
				List(decl)
			case label: IASTLabelStatement =>
				GotoLabel(label.getName.toString) +: compile(label.getNestedStatement)
			case exprState: CASTExpressionStatement =>
				List(exprState.getExpression)
			case _ =>
				node +: node.getChildren.toList
	}

	def flattenNode(tUnit: IASTNode)(implicit state: State): List[Any] = {
		tUnit.getChildren.flatMap(compile).toList
	}

	private def compileIfStatement(ifStatement: IASTIfStatement)(implicit state: State) = {
		val contents = PushVariableStack() +: compile(ifStatement.getThenClause) :+ PopVariableStack()
		val elseContents = PushVariableStack() +: List(Option(ifStatement.getElseClause)).flatten.flatMap(compile) :+ PopVariableStack()

		val jmp = if ifStatement.getElseClause != null then
			List(Jmp(elseContents.size))
		else
			List()

		val all = contents ++ jmp

		// add +1 for the jmp statement
		JmpIfNotEqual(ifStatement.getConditionExpression, all.size) +: (all ++ elseContents)
	}

	private def compileForStatement(forStatement: IASTForStatement)(implicit state: State) = {
		val breakLabel = BreakLabel()
		state.breakLabelStack = breakLabel +: state.breakLabelStack
		val continueLabel = ContinueLabel()
		state.continueLabelStack = continueLabel +: state.continueLabelStack

		val init = List(forStatement.getInitializerStatement)
		val contents = compile(forStatement.getBody)
		val iter = forStatement.getIterationExpression
		val beginLabel = GotoLabel("")

		state.breakLabelStack = state.breakLabelStack.tail
		state.continueLabelStack = state.continueLabelStack.tail

		val iterExpr = if iter != null then List(iter) else List()
		val execution = (contents :+ continueLabel) ++ iterExpr

		val jmpnz = if forStatement.getConditionExpression != null then
			List(JmpToLabelIfNotZero(forStatement.getConditionExpression, breakLabel))
		else
			List()

		val start = PushVariableStack() +: init :+ beginLabel
		val end = List(JmpLabel(beginLabel), breakLabel, PopVariableStack())

		start ++ jmpnz ++ execution ++ end
	}

	private def compileWhileStatement(whileStatement: IASTWhileStatement)(implicit state: State) = {
		val breakLabel = BreakLabel()
		state.breakLabelStack = breakLabel +: state.breakLabelStack
		val continueLabel = ContinueLabel()
		state.continueLabelStack = continueLabel +: state.continueLabelStack

		val contents = compile(whileStatement.getBody)
		val begin = GotoLabel("")
		val end = GotoLabel("")

		state.breakLabelStack = state.breakLabelStack.tail
		state.continueLabelStack = state.continueLabelStack.tail

		val result = List(JmpLabel(end), begin) ++ contents ++ List(end, continueLabel, JmpToLabelIfZero(whileStatement.getCondition, begin), breakLabel)

		PushVariableStack() +: result :+ PopVariableStack()
	}

	private def compileDoWhileStatement(doWhileStatement: IASTDoStatement)(implicit state: State) = {
		val breakLabel = BreakLabel()
		state.breakLabelStack = breakLabel +: state.breakLabelStack
		val continueLabel = ContinueLabel()
		state.continueLabelStack = continueLabel +: state.continueLabelStack

		val contents = compile(doWhileStatement.getBody)
		val begin = new Label {}

		state.breakLabelStack = state.breakLabelStack.tail
		state.continueLabelStack = state.continueLabelStack.tail

		val result = begin +: (contents ++ List(continueLabel, JmpToLabelIfZero(doWhileStatement.getCondition, begin), breakLabel))

		PushVariableStack() +: result :+ PopVariableStack()
	}

	private def compileSwitcheStatement(switch: IASTSwitchStatement)(implicit state: State) = {
		val breakLabel = BreakLabel()
		state.breakLabelStack = breakLabel +: state.breakLabelStack

		val descendants = compile(switch.getBody)

		@tailrec
		def getParentSwitchBody(node: IASTNode): IASTStatement = node.getParent match {
			case switch: IASTSwitchStatement => switch.getBody
			case _ => getParentSwitchBody(node.getParent)
		}

		val jumpTable = descendants.flatMap {
			case x@CaseLabel(caseStatement) if switch.getBody == getParentSwitchBody(caseStatement) =>
				val cached = CachedRValue(switch.getControllerExpression)
				cached +: List(JmpToLabelIfEqual(caseStatement.getExpression, cached, x))
			case x@DefaultLabel(default) if switch.getBody == getParentSwitchBody(default) =>
				List(JmpLabel(x))
			case _ =>
				List()
		} :+ JmpLabel(breakLabel)

		state.breakLabelStack = state.breakLabelStack.tail

		val result = jumpTable ++ descendants :+ breakLabel

		PushVariableStack() +: result :+ PopVariableStack()
	}

	private def compileCompoundStatement(compound: IASTCompoundStatement)(implicit state: State) = {
		val isTypicalCompound = compound.getParent match
			case _: (IASTSwitchStatement | CASTFunctionDefinition | CASTForStatement |
				CASTDoStatement | CASTWhileStatement) => true
			case _ => false

		if isTypicalCompound then
			compound.getStatements.flatMap(compile).toList
		else
			PushVariableStack() +: compound.getStatements.flatMap(compile).toList :+ PopVariableStack()
	}

	def parseCode(codes: Seq[String], includePaths: List[String]): List[IASTTranslationUnit] = {
		codes.map { code => Utils.getTranslationUnit(code, includePaths) }.toList
	}
}

class State(val sources: List[IASTTranslationUnit], val pointerSize: NumBits) {

	val Stack = Memory(40000)

	private var heapInsertIndex = 20000

	var functionContexts = List[FunctionScope]()

	def context: FunctionScope = functionContexts.head

	val functionList = ListBuffer[Function]()
	val functionPointers = scala.collection.mutable.LinkedHashMap[String, Variable]()
	val stdout = ListBuffer[Char]()

	private var breakLabelStack = List[Label]()
	private var continueLabelStack = List[Label]()

	val structs = sources.flatMap { src =>
		src.getDeclarations.collect { case simp: CASTSimpleDeclaration => simp.getDeclSpecifier }
			.collect { case comp: CASTCompositeTypeSpecifier => comp }
			.map { x => x.getName.resolveBinding().asInstanceOf[CStructure] }
	}

	val pointerType = pointerSize match
		case NumBits.ThirtyTwoBits => TypeHelper.intType
		case NumBits.SixtyFourBits => CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG_LONG)

	val addressSize = TypeHelper.sizeof(pointerType)(using this)

	// ************************************************* //
	//                  Constructor                      //
	// ************************************************* //
	
	Functions.scalaFunctions.foreach(addScalaFunctionDef)

	pushScope(new FunctionScope(List(), null, null) {})
	
	def pushScope(scope: FunctionScope): Unit = {
		functionContexts = scope +: functionContexts
	}

	def getFunctionScope = {
		functionContexts.collect { case fcnScope: FunctionScope => fcnScope }.head
	}

	def parseGlobals(tUnits: List[IASTNode]) = {
		val program = new FunctionScope(List(), null, null) {}
		pushScope(program)
		program.init(tUnits, this, false)

		context.run(this) // parse globals

		context.setAddress(0)
	}

	def popFunctionContext = {
		Stack.insertIndex = functionContexts.head.startingStackAddr
		functionContexts = functionContexts.tail
	}

	def hasFunction(name: String): Boolean = functionList.exists { fcn => fcn.name == name }

	def getFunctionByIndex(index: Int): Function = functionList.find { fcn => fcn.index == index }.get

	def addMain(sources: List[IASTTranslationUnit]) = {
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
		val newVar = Variable(fcn.name, State.this, fcnType)
		Stack.writeToMemory(count, newVar.address, fcnType)

		functionPointers += fcn.name -> newVar
	}

	private def addStaticFunctionVars(node: IASTNode)(implicit state: State): List[Variable] = {
		node match {
			case decl: IASTDeclarator =>
				val nameBinding = decl.getName.resolveBinding()

				nameBinding match {
					case vari: IVariable =>
						if (vari.isStatic) {
							val theType = TypeHelper.stripSyntheticTypeInfo(nameBinding.asInstanceOf[IVariable].getType)
							val variable = Variable(decl.getName.toString, state, vari.getType)
							
							if decl.getInitializer != null then
								val initVals = Declarator.getRValues(decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause, theType)
								Declarator.assign(variable, initVals, null, op_assign)

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

		functionList += new Function(name.toString, true) {
			index = count
			node = fcnDef
			override val staticVars = addStaticFunctionVars(fcnDef)(using State.this)

			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				None
			}
		}

		if (!isMain) {
			val newVar = Variable(name.toString, State.this, fcnType)
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

	def callTheFunction(name: String, call: IASTFunctionCallExpression, scope: Option[FunctionScope], isApi: Boolean = false)(implicit state: State): Option[ValueType] = {
		functionList.find(_.name == name).flatMap { function =>

			if (!function.isNative) {
				// this is a function simulated in scala

				val stackPos = Stack.insertIndex
				val args = call.getArguments.map { x => Expressions.evaluate(x) }

				val resolvedArgs: Array[RValue] = args.flatten.map(TypeHelper.toRValue)

				val returnVal = function.run(resolvedArgs.reverse, this)
				Stack.insertIndex = stackPos // pop the stack

				returnVal.map:
					case file @ FileRValue(_) => file
					case rValue => RValue(rValue.value, TypeHelper.unsignedIntType)
			} else {
				if (function.name == "main" && isApi) {
					scope.get.init(List(function.node), this, scope.isEmpty)
					functionContexts = List(scope.get)
					context.run(this)
					None
				} else {

					val newScope = scope.getOrElse:
						val expressionType = call.getExpressionType
						FunctionScope(function.staticVars, functionContexts.headOption.orNull, expressionType)

					newScope.init(List(function.node), this, scope.isEmpty)

					val args: List[ValueType] = call.getArguments.map { x => Expressions.evaluate(x).head }.toList

					val resolvedArgs = args.map(TypeHelper.toRValue)

					// printf assumes all floating point numbers are doubles
					val promoted = resolvedArgs.map: arg =>
						arg.theType match
							case basic: IBasicType if basic.getKind == IBasicType.Kind.eFloat => RValue(arg.value, TypeHelper.doubleType)
							case _ => arg

					newScope.pushOntoStack(promoted)
					newScope.pushOntoStack(RValue(resolvedArgs.size, TypeHelper.unsignedIntType))

					functionContexts = newScope +: functionContexts

					newScope.run(this)
					newScope.getReturnValue.map { retVal =>
						val valuesToPush: Option[Array[Byte]] = retVal match
							case structure @ LValue(_, _: CStructure) =>
								Some(structure.toByteArray)
							case _ => None

						popFunctionContext

						valuesToPush.foreach: byteArray =>
							val newAddr = state.allocateSpace(byteArray.length)
							state.writeDataBlock(byteArray, newAddr)

						retVal
					}.orElse {
						popFunctionContext
						None
					}
				}
			}
		}
	}

	def allocateSpace(numBytes: Int): Int = {
		val result = Stack.insertIndex
		Stack.insertIndex += Math.max(0, numBytes)
		result
	}

	def allocateHeapSpace(numBytes: Int): Int = {
		val result = heapInsertIndex
		heapInsertIndex += Math.max(0, numBytes)
		result
	}

	def copy(dst: Int, src: Int, numBytes: Int) = {
		Stack.tape.copy(dst, src, numBytes: Int)
	}

	def set(dst: Int, value: Byte, numBytes: Int) = {
		Stack.tape.set(dst, value, numBytes: Int)
	}

	def writeDataBlock(array: Array[Byte], startingAddress: Int)(implicit state: State): Unit = {
		Stack.tape.writeDataBlock(array, startingAddress)
	}

	def readDataBlock(startingAddress: Int, length: Int)(implicit state: State): Array[Byte] = {
		Stack.tape.readDataBlock(startingAddress, length)
	}

	def readPtrVal(address: Int): Int = {
		Stack.tape.readPtrVal(address)
	}

	private def stripQuotes(str: String): String = {
		str.tail.reverse.tail.reverse
	}

	def getString(str: String): RValue = {
		val theStr = stripQuotes(str)

		val withNull = (theStr.toCharArray :+ 0.toChar).map(_.toByte) // terminating null char
		val strAddr = allocateSpace(withNull.length)

		writeDataBlock(withNull, strAddr)(using this)
		RValue(strAddr, pointerType)
	}

	def createStringArrayVariable(varName: String, str: String): Variable = {
		val theStr = stripQuotes(str)
		val translateLineFeed = theStr.replace("\\n", 10.asInstanceOf[Char].toString)
		val withNull = (translateLineFeed.toCharArray :+ 0.toChar)
			.map { char => RValue(char.toByte, TypeHelper.charType) }.toList // terminating null char

		val inferredArrayType = CArrayType(TypeHelper.charType)
		inferredArrayType.setModifier(CASTArrayModifier(CASTLiteralExpression(IASTLiteralExpression.lk_integer_constant, str.length.toString.toCharArray)))

		val theArrayPtr = context.addArrayVariable(varName, inferredArrayType, withNull)
		theArrayPtr
	}

	def writeDataBlock(array: List[RValue], startingAddress: Int)(implicit state: State): Unit = {
		var address = startingAddress

		array.foreach:
			case RValue(newVal, theType) =>
				Stack.writeToMemory(newVal, address, theType)
				address += TypeHelper.sizeof(theType)(using state)
	}
}
