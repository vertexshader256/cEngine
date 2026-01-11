package scala.c.engine

import org.eclipse.cdt.core.dom.ast.{IASTCaseStatement, IASTDefaultStatement, IASTExpression}
import scala.c.engine.models.*

trait CEngineInstruction

object Instructions {
	case class ReturnFromFunction() extends Exception("returning")

	case class CachedRValue(expr2: IASTExpression) extends CEngineInstruction {
		var cachedValue: RValue = null
	}

	case class JmpIfNotEqual(expr: IASTExpression, relativeJump: Int) extends CEngineInstruction

	case class JmpToLabelIfNotZero(expr: IASTExpression, label: Label) extends CEngineInstruction

	case class JmpToLabelIfZero(expr: IASTExpression, label: Label) extends CEngineInstruction

	case class JmpToLabelIfEqual(expr1: IASTExpression, expr2: CachedRValue, label: Label) extends CEngineInstruction

	case class Jmp(relativeJump: Int) extends CEngineInstruction

	case class JmpLabel(label: Label) extends CEngineInstruction

	case class JmpName(label: String) extends CEngineInstruction {
		var destAddress = 0
	}

	abstract class Label extends CEngineInstruction {
		var address = 0
	}

	case class PushVariableStack() extends CEngineInstruction

	case class PopVariableStack() extends CEngineInstruction

	case class GotoLabel(name: String) extends Label

	case class BreakLabel() extends Label

	case class ContinueLabel() extends Label

	case class CaseLabel(caseStatement: IASTCaseStatement) extends Label

	case class DefaultLabel(default: IASTDefaultStatement) extends Label
}
