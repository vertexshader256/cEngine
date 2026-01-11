package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.op_assign
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*
import scala.c.engine.models.*
import scala.c.engine.ast.BinaryExpr.evaluate
import scala.util.Try

object Declarator {

	def execute(decl: IASTDeclarator)(implicit state: State): Any = decl match {
		case fcnDec: IASTFunctionDeclarator =>
			processFcnDeclarator(fcnDec)
		case arrayDecl: IASTArrayDeclarator =>
			processArrayDecl(arrayDecl)
		case decl: CASTDeclarator =>
			processCastDecl(decl)
	}

	def getRValues(decl: IASTInitializerClause, theType: IType)(implicit state: State): List[ValueType] = {
		theType match
			case struct: CStructure =>
				getValuesFromInitializer(decl, struct)
			case _ =>
				List(Expressions.evaluate(decl).get)
	}

	def assign(dst: LValue, srcs: List[ValueType], equals: IASTInitializerClause, op: Int)(implicit state: State): Unit = {
		if !dst.theType.isInstanceOf[CStructure] then
			val result = evaluate(dst, srcs.head, op) match
				case file @ FileRValue(_) => file
				case x => TypeHelper.cast(x.value, dst.theType)

			dst.setValue(result)
		else
			equals match
				case _: IASTFunctionCallExpression =>
					state.copy(dst.address, state.Stack.insertIndex - dst.sizeof, dst.sizeof)
				case _: IASTTypeIdInitializerExpression =>
					val otherStruct = Expressions.evaluate(equals).get.asInstanceOf[LValue]
					state.copy(dst.address, otherStruct.address, dst.sizeof)
				case _: IASTExpression =>
					val otherStruct = srcs.head.asInstanceOf[LValue]
					state.copy(dst.address, otherStruct.address, dst.sizeof)
				case _ =>
					val struct = dst.theType.asInstanceOf[CStructure]
					struct.getFields.zip(srcs).foreach:
						case (field, newValue) =>
							val theField = Structures.offsetof(struct, dst.address, field.getName, state)
							assign(theField, List(newValue), equals, op)
	}

	private def setFunctionPointer(fcnDec: IASTFunctionDeclarator)(implicit state: State): Unit = {
		// when you're initializing a function pointer: int (*funcPtr2)(int, int) = blah2;
		val nameBinding = fcnDec.getNestedDeclarator.getName.resolveBinding()
		val name = fcnDec.getNestedDeclarator.getName

		nameBinding match
			case vari: IVariable =>
				val theType = TypeHelper.stripSyntheticTypeInfo(vari.getType)
				val variable = state.context.addVariable(name.toString, theType)
				Ast.step(fcnDec.getInitializer)
				variable.setValue(TypeHelper.toRValue(state.context.popStack))
	}

	private def writeFcnArguments(fcnDec: IASTFunctionDeclarator)(implicit state: State): Array[IASTNode] = {
		val others = fcnDec.getChildren.filter { x => !x.isInstanceOf[IASTParameterDeclaration] && !x.isInstanceOf[IASTName] }

		val isInFunctionPrototype = !Utils.getAncestors(fcnDec).exists(_.isInstanceOf[IASTFunctionDefinition])

		if (!isInFunctionPrototype) {
			val numArgs = state.context.popStack.asInstanceOf[RValue].value.asInstanceOf[Integer]
			val args = (0 until numArgs).map { _ => state.context.popStack }.reverse
			
			val binding = fcnDec.getName.resolveBinding()
			val fcn = binding.asInstanceOf[CFunction]
			val paramDecls = fcn.getParameters.toList
			val zipped = args.zip(paramDecls)

			zipped.foreach { (arg, param) =>
				if arg.isInstanceOf[Variable] && arg.asInstanceOf[Variable].aType.isInstanceOf[CStructure] then
					// copying a structure by value
					val copy = Structures.copyStructure(arg.asInstanceOf[Variable], state)
					state.context.addVariable(copy)
				else
					val resolvedArg = TypeHelper.toRValue(arg)
					val newVar = state.context.addVariable(param.getName, param.getType)
					val casted = TypeHelper.cast(resolvedArg.value, newVar.theType).value
					state.Stack.writeToMemory(casted, newVar.address, newVar.theType)
			}
		}

		others
	}

	private def processFcnDeclarator(fcnDec: IASTFunctionDeclarator)(implicit state: State): Unit = {
		if (Utils.getDescendants(fcnDec).exists { x => x.isInstanceOf[IASTEqualsInitializer] }) {
			setFunctionPointer(fcnDec)
		} else {
			val binding = fcnDec.getName.resolveBinding()

			binding match
				case fcn: CFunction if fcn.getParameters.nonEmpty => writeFcnArguments(fcnDec)
				case _ => Seq()
		}
	}

	private def flattenInitList(node: IASTInitializerClause)(implicit state: State): List[ValueType] = node match {
		case list: IASTInitializerList =>
			list.getClauses.toList.flatMap(flattenInitList)
		case lit: IASTLiteralExpression =>
			List(Expressions.evaluate(lit).get)
		case unary: IASTUnaryExpression =>
			List(Expressions.evaluate(unary).get)
		case id: IASTIdExpression =>
			val variable = Expressions.evaluate(id).get.asInstanceOf[Variable]
			List(variable.rValue)
		case bin: IASTBinaryExpression =>
			Expressions.evaluate(bin).get match
				case variable: Variable => List(variable.rValue)
				case rVal: RValue => List(rVal)
	}

	private def createdSizedArrayType(theType: CArrayType, dimensions: List[Int]): CArrayType = {
		val theArrayType = theType.getType match
			case array: CArrayType => createdSizedArrayType(theType.getType.asInstanceOf[CArrayType], dimensions.tail)
			case _ => theType.getType

		val arrayType = CArrayType(theArrayType)
		arrayType.setModifier(CASTArrayModifier(CASTLiteralExpression(IASTLiteralExpression.lk_integer_constant, dimensions.head.toString.toCharArray)))
		arrayType
	}

	private def initializeNullArray(name: IASTName, arrayDecl: IASTArrayDeclarator)(implicit state: State) = {
		val theType = TypeHelper.getBindingType(name.resolveBinding())

		val dimensions = arrayDecl.getArrayModifiers.toList.filter {
			_.getConstantExpression != null
		}.map { _ =>
			arrayDecl.getArrayModifiers.foreach(Ast.step)
			val value = TypeHelper.toRValue(state.context.popStack).value
			TypeHelper.cast(value, TypeHelper.intType).value.asInstanceOf[Int]
		}

		val aType = theType match
			case array: CArrayType if dimensions.nonEmpty => createdSizedArrayType(array, dimensions.reverse)
			case _ => theType

		state.context.addVariable(name.toString, aType)
	}

	private def processList(theType: IType, list: CASTInitializerList)(implicit state: State): List[RValue] = {
		val flattened = flattenInitList(list).map(TypeHelper.toRValue)

		if !TypeHelper.isPointer(theType) && !Structures.isStructure(theType) then
			val baseType = TypeHelper.resolveBasic(theType)
			flattened.map { x => TypeHelper.cast(x.value, baseType) }
		else
			flattened
	}

	private def initializeArrayVariable(name: IASTName, init: IASTInitializerClause)(implicit state: State): Variable = {
		val theType = TypeHelper.getBindingType(name.resolveBinding())
		val pointerType = TypeHelper.getPointerType(theType)

		val values = pointerType match
			case struct: CStructure => // array of structs
				init.getChildren.flatMap { list =>
					getValuesFromInitializer(list.asInstanceOf[IASTInitializerClause], struct).map(TypeHelper.toRValue)
				}.toList
			case _ =>
				processList(theType, init.asInstanceOf[CASTInitializerList])

		state.context.addArrayVariable(name.toString, theType, values)
	}

	private def processArrayDecl(arrayDecl: IASTArrayDeclarator)(implicit state: State): Unit = {
		
		val name = if arrayDecl.getNestedDeclarator != null then
			arrayDecl.getNestedDeclarator.getName
		else
			arrayDecl.getName

		if (arrayDecl.getInitializer != null) {
			val equals = arrayDecl.getInitializer.asInstanceOf[IASTEqualsInitializer]
			val hasList = equals.getInitializerClause.isInstanceOf[IASTInitializerList]

			if (hasList) {
				val init = equals.getInitializerClause
				initializeArrayVariable(name, init)
			} else {
				val theType = TypeHelper.getBindingType(name.resolveBinding())

				val stringType = TypeHelper.resolveBasic(theType)
				if (stringType.getKind == IBasicType.Kind.eChar) {
					// e.g. char str[] = "Hello!\n";
					List(Option(arrayDecl.getInitializer)).flatten.foreach(Ast.step)
					val initString = state.context.popStack.asInstanceOf[StringLiteral].value
					state.createStringArrayVariable(name.toString, initString, stringType)
				} else { // initializing array to address, e.g int (*ptr)[5] = &x[1];
					Ast.step(arrayDecl.getInitializer)
					val initVal = TypeHelper.toRValue(state.context.popStack)
					val newArray = List(initVal)
					state.context.addArrayVariable(name.toString, theType, newArray)
				}
			}
		} else {
			initializeNullArray(name, arrayDecl) // no initializer
		}
	}

	private def processCastDecl(decl: CASTDeclarator)(implicit state: State): Unit = {
		val nameBinding = decl.getName.resolveBinding()
		val name = decl.getName

		nameBinding match {
			case variable: IVariable =>
				val theType = TypeHelper.stripSyntheticTypeInfo(variable.getType)

				val addedVariable = if variable.isExtern then
					state.context.addExternVariable(name.toString, theType)
				else
					state.context.addVariable(name.toString, theType)

				if (!addedVariable.isInitialized) {
					decl.getInitializer match
						case equals: IASTEqualsInitializer =>
							val initClause = equals.getInitializerClause
							val initVals = getRValues(initClause, theType)
							assign(addedVariable, initVals, initClause, op_assign)
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

	def getValuesFromList(list: IASTInitializerList, theType: IType)(implicit state: State): List[ValueType] = {
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

	private def getValuesFromInitializer(initClause: IASTInitializerClause, theType: IType)(implicit state: State): List[ValueType] = {
		initClause match
			case list: IASTInitializerList =>
				getValuesFromList(list, theType)
			case idExpr: IASTIdExpression =>
				List(state.context.resolveId(idExpr.getName).get)
			case fcnCall: IASTFunctionCallExpression =>
				Ast.step(initClause)
				state.context.popStack
				List()
			case _ =>
				List()
	}
}
