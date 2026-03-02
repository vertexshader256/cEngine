package scala.c.engine

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.op_assign
import org.eclipse.cdt.internal.core.dom.parser.c.*

import scala.c.engine.ast.{Declarator, Expressions}
import scala.c.engine.cFunctions.*
import scala.c.engine.instructions.*
import scala.c.engine.models.*
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait Functions {
	this: CEngine =>

	var varArgStartingAddr = List[Int]()
	private val scalaFunctions = ListBuffer[Function]()
	protected val functionList = ListBuffer[Function]()
	val functionPointers = scala.collection.mutable.LinkedHashMap[String, Variable]()
	private val functionContexts = mutable.Stack[FunctionScope]()

	def context: FunctionScope = functionContexts.head

	def getFunctionScope: FunctionScope = {
		functionContexts.collect { case fcnScope: FunctionScope => fcnScope }.head
	}

	def loadFunctions() = {
		Stdio.addFunctions(scalaFunctions)(using this)
		Mathh.addFunctions(scalaFunctions)(using this)
		Stdlibh.addFunctions(scalaFunctions)(using this)
		Stringh.addFunctions(scalaFunctions)(using this)
		Stdargh.addFunctions(scalaFunctions)

		scalaFunctions.foreach(addScalaFunctionDef)
	}

	def functionCallExpr(call: IASTFunctionCallExpression): Option[ValueType] = {
		val pop = Expressions.evaluate(call.getFunctionNameExpression)(this).head

		val name = if (hasFunction(call.getFunctionNameExpression.getRawSignature)) {
			call.getFunctionNameExpression.getRawSignature
		} else {
			val info = pop.asInstanceOf[LValue]
			val resolved = TypeHelper.stripSyntheticTypeInfo(info.theType)
			resolved match {
				case _: IPointerType => getFunctionByIndex(info.rValue.value.asInstanceOf[Int]).name
			}
		}

		callTheFunction(name, call, None)
	}

	private def runEmulatedFunction(function: Function, call: IASTFunctionCallExpression) = {
		val stackPos = memory.getStackPosition
		val args = call.getArguments.map { x => Expressions.evaluate(x)(using this) }

		val resolvedArgs: Array[RValue] = args.flatten.map(TypeHelper.toRValue(_)(using this))

		val returnVal = function.run(resolvedArgs.reverse, this)
		memory.setStackPosition(stackPos) // pop the stack

		returnVal.map:
			case file@FileRValue(_) => file
			case rValue => RValue(rValue.value, TypeHelper.unsignedIntType)
	}

	private def runDefinedFunction(function: Function, call: IASTFunctionCallExpression, scope: Option[FunctionScope], isApi: Boolean): Option[ValueType] = {
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
				case structure@LValue(_, structType: CStructure) =>
					val structBytes = structure.toByteArray
					val newAddr = allocateStack(structBytes.length)
					writeDataBlock(newAddr, structBytes)
					Structure(structBytes, structType)
				case retVal => retVal
			}.orElse {
				None
			}
		}
	}

	private def runFunction(function: Function, call: IASTFunctionCallExpression, scope: Option[FunctionScope], isApi: Boolean) = {
		if (!function.isNative) {
			// this is a function simulated in scala
			runEmulatedFunction(function, call)
		} else {
			runDefinedFunction(function, call, scope, isApi)
		}
	}

	def callTheFunction(name: String, call: IASTFunctionCallExpression, scope: Option[FunctionScope], isApi: Boolean = false): Option[ValueType] = {
		functionList.find(_.name == name).flatMap { function =>
			runFunction(function, call, scope, isApi)
		}
	}

	protected def pushScope(scope: FunctionScope): Unit = {
		functionContexts.push(scope)
	}

	def writeFcnArguments(fcnDec: IASTFunctionDeclarator): Unit = {
		val isInFunctionPrototype = !Utils.getAncestors(fcnDec).exists(_.isInstanceOf[IASTFunctionDefinition])

		if (!isInFunctionPrototype) {
			writeFunctionStackFrame(fcnDec)
		}
	}

	private def hasFunction(name: String): Boolean = functionList.exists { fcn => fcn.name == name }
	private def getFunctionByIndex(index: Int): Function = functionList.find { fcn => fcn.index == index }.get

	private def writeFunctionStackFrame(fcnDec: IASTFunctionDeclarator): Unit = {
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
					memory.writeToMemory(casted, newVar.address, newVar.theType)
			}
		}
	}

	protected def addFunctionDef(fcnDef: IASTFunctionDefinition, isMain: Boolean) = {
		val name = fcnDef.getDeclarator.getName
		val count = functionPointers.size

		val fcnType = fcnDef.getDeclarator.getName.resolveBinding().asInstanceOf[IFunction].getType

		val newFcn = new Function(name.toString, true) {
			index = count
			node = fcnDef

			def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = {
				None
			}
		}

		functionList += newFcn
		newFcn.run(null, null) // for coverage

		if (!isMain) {
			val newVar = Variable(name, this, fcnType)
			memory.writeToMemory(count, newVar.address, fcnType)

			functionPointers += name.toString -> newVar
		}
	}

	private def addScalaFunctionDef(fcn: Function) = {
		val count = functionPointers.size
		fcn.index = count

		functionList += fcn

		val fcnType = CFunctionType(CBasicType(IBasicType.Kind.eVoid, 0), null)
		val newVar = Variable(new CASTName(fcn.name.toCharArray), this, fcnType)
		memory.writeToMemory(count, newVar.address, fcnType)

		functionPointers += fcn.name -> newVar
	}

	private def popFunctionContext: FunctionScope = {
		val frame = functionContexts.pop()
		memory.setStackPosition(frame.startingStackAddr)
		frame
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
				newScope.pushOntoStack(resolved)
			}
		}

		newScope.pushOntoStack(RValue(args.size, TypeHelper.unsignedIntType))
		newScope
	}
}
