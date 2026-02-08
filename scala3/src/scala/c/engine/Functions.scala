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

trait Functions {
	this: State =>

	def hasFunction(name: String): Boolean = functionList.exists { fcn => fcn.name == name }

	def getFunctionByIndex(index: Int): Function = functionList.find { fcn => fcn.index == index }.get

	def addScalaFunctionDef(fcn: Function) = {
		val count = functionPointers.size
		fcn.index = count

		functionList += fcn

		val fcnType = CFunctionType(CBasicType(IBasicType.Kind.eVoid, 0), null)
		val newVar = Variable(new CASTName(fcn.name.toCharArray), this, fcnType)
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
}
