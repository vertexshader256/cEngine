package scala.c.engine.ast

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import scala.c.engine.{CEngine, TypeHelper, Utils}
import scala.c.engine.models.*
import scala.util.Try

object Initializer {

	extension (list: IASTInitializerList)
		def isNullInitializer: Boolean = {
			if (list.getClauses.length == 1) {
				val rawSig = list.getClauses.toList.head.getRawSignature
				Try(rawSig.toInt == 0).getOrElse(false)
			} else {
				false
			}
		}

	def getRValues(decl: IASTInitializerClause, theType: IType, isStatic: Boolean)(using CEngine): List[ValueType] = {
		theType match
			case struct: CStructure =>
				getValuesFromInitializer(decl, struct, isStatic)
			case _ =>
				List(Expressions.evaluate(decl).get)
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
		} else if (isStructure && list.isNullInitializer) {
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
