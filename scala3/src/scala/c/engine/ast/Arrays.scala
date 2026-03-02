package scala.c.engine.ast

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import scala.c.engine.models.*
import scala.c.engine.{CEngine, Structures, TypeHelper}

object Arrays {

	private def flattenInitList(node: IASTInitializerClause)(using CEngine): List[ValueType] = node match {
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

	private def processList(theType: IType, list: CASTInitializerList, isStatic: Boolean)(using CEngine): List[RValue] = {
		val flattened = flattenInitList(list).map(x => TypeHelper.toRValue(x, isStatic))

		if !TypeHelper.isPointer(theType) && !Structures.isStructure(theType) then
			val baseType = TypeHelper.resolveBasic(theType)
			flattened.map { x => TypeHelper.cast(x.value, baseType) }
		else
			flattened
	}

	private def initializeArrayVariable(name: IASTName, init: IASTInitializerClause)(implicit cEngine: CEngine): Variable = {
		val theType = TypeHelper.getBindingType(name.resolveBinding())
		val pointerType = TypeHelper.getPointerType(theType)

		val isStatic = name.resolveBinding() match
			case variable: CVariable => variable.isStatic

		val values = pointerType match
			case struct: CStructure => // array of structs
				init.getChildren.flatMap { list =>
					Initializer.getValuesFromInitializer(list.asInstanceOf[IASTInitializerClause], struct, isStatic).map(x => TypeHelper.toRValue(x))
				}.toList
			case _ =>
				processList(theType, init.asInstanceOf[CASTInitializerList], isStatic)

		cEngine.context.addVariable(name, theType, values)
	}

	def createArrayVariable(name: IASTName, arrayDecl: IASTArrayDeclarator)(implicit cEngine: CEngine) = {
		val equals = arrayDecl.getInitializer.asInstanceOf[IASTEqualsInitializer]
		val hasList = equals.getInitializerClause.isInstanceOf[IASTInitializerList]

		if (hasList) {
			val init = equals.getInitializerClause
			initializeArrayVariable(name, init)
		} else {
			val theType = TypeHelper.getBindingType(name.resolveBinding())

			val stringType = TypeHelper.resolveBasic(theType)
			Ast.step(arrayDecl.getInitializer)

			if (stringType.getKind == IBasicType.Kind.eChar) {
				// e.g. char str[] = "Hello!\n";
				val initString = cEngine.context.popStack.asInstanceOf[StringLiteral].value
				cEngine.createStringArrayVariable(name, initString, stringType)
			} else { // initializing array to address, e.g int (*ptr)[5] = &x[1];
				val initVal = TypeHelper.toRValue(cEngine.context.popStack)
				val newArray = List(initVal)
				cEngine.context.addVariable(name, theType, newArray)
			}
		}
	}

	def processArrayDecl(arrayDecl: IASTArrayDeclarator)(implicit cEngine: CEngine): Unit = {
		val decl = if arrayDecl.getNestedDeclarator != null then
			arrayDecl.getNestedDeclarator
		else
			arrayDecl

		val name = decl.getName

		if (!cEngine.context.isStaticAlreadyDefined(name)) {
			if (arrayDecl.getInitializer != null) {
				createArrayVariable(name, arrayDecl)
			} else {
				initializeNullArray(name, arrayDecl) // no initializer
			}
		}
	}

	private def createdSizedArrayType(theType: CArrayType, dimensions: List[Int]): CArrayType = {
		val theArrayType = theType.getType match
			case array: CArrayType => createdSizedArrayType(theType.getType.asInstanceOf[CArrayType], dimensions.tail)
			case _ => theType.getType

		val arrayType = CArrayType(theArrayType)
		arrayType.setModifier(CASTArrayModifier(CASTLiteralExpression(IASTLiteralExpression.lk_integer_constant, dimensions.head.toString.toCharArray)))
		arrayType
	}

	private def initializeNullArray(name: IASTName, arrayDecl: IASTArrayDeclarator)(implicit cEngine: CEngine) = {
		val theType = TypeHelper.getBindingType(name.resolveBinding())

		val dimensions = arrayDecl.getArrayModifiers.toList.filter {
			_.getConstantExpression != null
		}.map { _ =>
			arrayDecl.getArrayModifiers.foreach(Ast.step)
			val value = TypeHelper.toRValue(cEngine.context.popStack).value
			TypeHelper.cast(value, TypeHelper.intType).value.asInstanceOf[Int]
		}

		val aType = theType match
			case array: CArrayType if dimensions.nonEmpty => createdSizedArrayType(array, dimensions.reverse)
			case _ => theType

		cEngine.context.addVariable(name, aType)
	}
}
