package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.op_assign
import org.eclipse.cdt.internal.core.dom.parser.c.*

import scala.c.engine.ast.BinaryExpr.evaluate
import scala.c.engine.models.*
import scala.util.Try

object Declarator {

	def execute(decl: IASTDeclarator)(using CEngine): Any = decl match {
		case fcnDec: IASTFunctionDeclarator =>
			processFcnDeclarator(fcnDec)
		case arrayDecl: IASTArrayDeclarator =>
			Arrays.processArrayDecl(arrayDecl)
		case decl: CASTDeclarator =>
			processDeclarator(decl)
	}

	private def getRValues(decl: IASTInitializerClause, theType: IType, isStatic: Boolean)(using CEngine): List[ValueType] = {
		theType match
			case struct: CStructure =>
				getValuesFromInitializer(decl, struct, isStatic)
			case _ =>
				List(Expressions.evaluate(decl).get)
	}

	def assign(dst: LValue, srcs: List[ValueType], equals: IASTInitializerClause, op: Int, isStatic: Boolean = false)(implicit cEngine: CEngine): Unit = {
		if !dst.theType.isInstanceOf[CStructure] then
			val eval = evaluate(dst, srcs.head, op, isStatic)
			val result = eval match
				case file @ FileRValue(_) => file
				case x => TypeHelper.cast(x.value, dst.theType)

			dst.setValue(result)
		else
			equals match
				case _: IASTFunctionCallExpression =>
					cEngine.copy(dst.address, Address(cEngine.memory.getStackPosition - dst.sizeof), dst.sizeof)
				case _: IASTTypeIdInitializerExpression =>
					val otherStruct = Expressions.evaluate(equals).get.asInstanceOf[LValue]
					cEngine.copy(dst.address, otherStruct.address, dst.sizeof)
				case _: IASTExpression =>
					val otherStruct = srcs.head.asInstanceOf[LValue]
					cEngine.copy(dst.address, otherStruct.address, dst.sizeof)
				case _ =>
					val struct = dst.theType.asInstanceOf[CStructure]
					struct.getFields.zip(srcs).foreach:
						case (field, newValue) =>
							val theField = Structures.offsetof(struct, dst.address, field.getName, cEngine)
							assign(theField, List(newValue), equals, op, isStatic)
	}

	private def setFunctionPointer(fcnDec: IASTFunctionDeclarator)(implicit cEngine: CEngine): Unit = {
		// when you're initializing a function pointer: int (*funcPtr2)(int, int) = blah2;
		val nameBinding = fcnDec.getNestedDeclarator.getName.resolveBinding()
		val name = fcnDec.getNestedDeclarator.getName

		nameBinding match
			case vari: IVariable =>
				val theType = TypeHelper.stripSyntheticTypeInfo(vari.getType)
				val variable = cEngine.context.addVariable(name, theType)
				Ast.step(fcnDec.getInitializer)
				variable.setValue(TypeHelper.toRValue(cEngine.context.popStack))
	}

	private def processFcnDeclarator(fcnDec: IASTFunctionDeclarator)(implicit cEngine: CEngine): Unit = {
		if (Utils.getDescendants(fcnDec).exists { x => x.isInstanceOf[IASTEqualsInitializer] }) {
			setFunctionPointer(fcnDec)
		} else {
			val binding = fcnDec.getName.resolveBinding()

			binding match
				case fcn: CFunction if fcn.getParameters.nonEmpty => cEngine.writeFcnArguments(fcnDec)
				case _ => Seq()
		}
	}

	// where variables get created
	private def processDeclarator(decl: CASTDeclarator)(implicit cEngine: CEngine): Unit = {
		val nameBinding = decl.getName.resolveBinding()
		val name = decl.getName

		nameBinding match {
			case variable: IVariable =>
				val theType = TypeHelper.stripSyntheticTypeInfo(variable.getType)

				val addedVariable = if variable.isExtern then
					cEngine.context.addExternVariable(name, theType)
				else
					cEngine.context.addVariable(name, theType)

				if (!addedVariable.isInitialized) {
					decl.getInitializer match
						case equals: IASTEqualsInitializer =>
							val initClause = equals.getInitializerClause
							val initVals = getRValues(initClause, theType, addedVariable.isStatic)
							assign(addedVariable, initVals, initClause, op_assign, addedVariable.isStatic)
						case _ =>

					addedVariable.isInitialized = true
				}
			case _ =>
		}
	}

	// should return 'true' if this list is equivilant to {0}
	private def isNullInitializer(list: IASTInitializerList): Boolean = {
		if (list.getClauses.length == 1) {
			val rawSig = list.getClauses.toList.head.getRawSignature
			Try(rawSig.toInt == 0).getOrElse(false)
		} else {
			false
		}
	}

	private def getValuesFromList(list: IASTInitializerList, theType: IType, isStatic: Boolean)(using CEngine): List[ValueType] = {
		val descendants = Utils.getDescendants(list)
		val hasNamedDesignator = descendants.exists { node => node.isInstanceOf[CASTDesignatedInitializer] } // {.y = 343, .x = 543, .next = 8578}
		val isStructure = theType.isInstanceOf[CStructure]

		if (isStructure && hasNamedDesignator) {
			val struct = theType.asInstanceOf[CStructure]
			val initializers = descendants.collect { case des: CASTDesignatedInitializer => des }
			val initValues = initializers.map: init =>
				val fieldName = init.getDesignators.toList.head.asInstanceOf[CASTFieldDesignator].getName.toString
				(fieldName, Expressions.evaluate(init.getOperand).get)
			.toMap

			struct.getFields.map { field =>
				initValues.getOrElse(field.getName, TypeHelper.zero)
			}.toList
		} else if (isStructure && isNullInitializer(list)) {
			val struct = theType.asInstanceOf[CStructure]
			struct.getFields.toList.map(x => TypeHelper.zero)
		} else {
			list.getClauses.map(Expressions.evaluateAndResolveVariable).toList
		}
	}

	def getValuesFromInitializer(initClause: IASTInitializerClause, theType: IType, isStatic: Boolean)(implicit cEngine: CEngine): List[ValueType] = {
		initClause match
			case list: IASTInitializerList =>
				getValuesFromList(list, theType, isStatic)
			case idExpr: IASTIdExpression =>
				List(cEngine.context.resolveId(idExpr.getName).get)
			case fcnCall: IASTFunctionCallExpression =>
				Ast.step(initClause)
				cEngine.context.popStack
				List()
			case _ =>
				List()
	}
}
