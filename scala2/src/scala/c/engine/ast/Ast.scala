package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.c.engine.Instructions._

object Ast {

	def executeCustomInstructions(current: Any)(implicit state: State): Unit = current match {
		case PushVariableStack() =>
			state.context.pushVariableScope()
		case PopVariableStack() =>
			state.context.popVariableScope()
		case cached@CachedRValue(expr) =>
			cached.cachedValue = TypeHelper.resolve(Expressions.evaluate(expr).get)
		case JmpIfNotEqual(expr, lines) =>
			val raw = Expressions.evaluate(expr).get
			val result = TypeHelper.resolveBoolean(raw)
			if (!result) {
				state.context.jmpRelative(lines)
			}
		case JmpToLabelIfNotZero(expr, label) =>
			val raw = Expressions.evaluate(expr).get
			val result = TypeHelper.resolveBoolean(raw)
			if (!result) {
				state.context.setAddress(label.address)
			}
		case JmpLabel(label) =>
			state.context.setAddress(label.address)
		case JmpToLabelIfZero(expr, label) =>
			val raw = Expressions.evaluate(expr).get
			val result = TypeHelper.resolveBoolean(raw)
			if (result) {
				state.context.setAddress(label.address)
			}
		case JmpToLabelIfEqual(expr1, cached, label) =>
			val raw1 = TypeHelper.resolve(Expressions.evaluate(expr1).get).value
			val raw2 = cached.cachedValue.value
			if (raw1 == raw2) {
				state.context.setAddress(label.address)
			}
		case Jmp(lines) =>
			state.context.jmpRelative(lines)
		case jmp: JmpName =>
			state.context.setAddress(jmp.destAddress)
		case label: Label =>
	}

	def step(current: Any)(implicit state: State): Unit = current match {

		case statement: IASTStatement =>
			Statement.parse(statement)
		case expression: IASTExpression => {
			Expressions.evaluate(expression).foreach { value =>
				state.context.pushOntoStack(value)
			}
			Seq()
		}
		case decl: IASTDeclarator =>
			Declarator.execute(decl)
		case array: IASTArrayModifier => {
			List(Option(array.getConstantExpression)).flatten.foreach(step)
		}

		case simple: IASTSimpleDeclaration => {
			val declSpec = simple.getDeclSpecifier

			val isWithinFunction = Utils.getAncestors(simple).exists {
				_.isInstanceOf[IASTFunctionDefinition]
			}

			simple.getDeclarators.foreach {
				case fcn: IASTFunctionDeclarator =>
					if (!isWithinFunction || fcn.getInitializer != null) {
						step(fcn)
					} else {
						step(fcn.getNestedDeclarator)
					}
				case x => step(x)
			}

			if (declSpec.isInstanceOf[IASTEnumerationSpecifier]) {
				step(simple.getDeclSpecifier)
			}
		}
		case enumer: IASTEnumerationSpecifier => {
			var current = 0
			enumer.getEnumerators.foreach {
				case enumerator: CASTEnumerator =>
					if (enumerator.getValue != null) {
						val value = Expressions.evaluate(enumerator.getValue).get.asInstanceOf[RValue]

						val newVar = state.context.addVariable(enumerator.getName.toString, TypeHelper.intType)
						current = value.value.asInstanceOf[Int] + 1
						state.Stack.writeToMemory(value.value, newVar.address, TypeHelper.intType)
					} else {
						val newVar = state.context.addVariable(enumerator.getName.toString, TypeHelper.intType)
						state.Stack.writeToMemory(current, newVar.address, TypeHelper.intType)
						current += 1
					}
			}
		}
		case fcnDef: IASTFunctionDefinition => {
		}
		case initList: IASTInitializerList => {
			initList.getClauses.foreach {
				step
			}
		}
		case equals: IASTEqualsInitializer => {
			step(equals.getInitializerClause)
		}
		case x => {
			executeCustomInstructions(x)
		}
	}
}
