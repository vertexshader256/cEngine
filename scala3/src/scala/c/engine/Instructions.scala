package scala.c.engine

import org.eclipse.cdt.core.dom.ast.{IASTCaseStatement, IASTDefaultStatement, IASTExpression}
import scala.c.engine.models.*

object Instructions {
	case class ReturnFromFunction() extends Exception("returning")

	case class CachedRValue(expr2: IASTExpression) {
		var cachedValue: RValue = null
	}

	case class JmpIfNotEqual(expr: IASTExpression, relativeJump: Int)

	case class JmpToLabelIfNotZero(expr: IASTExpression, label: Label)

	case class JmpToLabelIfZero(expr: IASTExpression, label: Label)

	case class JmpToLabelIfEqual(expr1: IASTExpression, expr2: CachedRValue, label: Label)

	case class Jmp(relativeJump: Int)

	case class JmpLabel(label: Label)

	case class JmpName(label: String) {
		var destAddress = 0
	}

	abstract class Label {
		var address = 0
	}

	case class PushVariableStack()

	case class PopVariableStack()

	case class GotoLabel(name: String) extends Label

	case class BreakLabel() extends Label

	case class ContinueLabel() extends Label

	case class CaseLabel(caseStatement: IASTCaseStatement) extends Label

	case class DefaultLabel(default: IASTDefaultStatement) extends Label
}
