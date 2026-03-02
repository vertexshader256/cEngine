package scala.c.engine

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import scala.annotation.tailrec
import scala.c.engine.instructions.*

trait Compiler {

	private var breakLabelStack = List[Label]()
	private var continueLabelStack = List[Label]()

	def compile(tUnit: IASTNode)(using CEngine): List[IASTNode | CEngineInstruction] = {
		tUnit.getChildren.flatMap(compileNode).toList
	}

	private def compileNode(node: IASTNode)(implicit cEngine: CEngine): List[IASTNode | CEngineInstruction] = {
		node match
			case ifStatement: IASTIfStatement =>
				compileIfStatement(ifStatement)
			case forStatement: IASTForStatement =>
				compileForStatement(forStatement)
			case whileStatement: IASTWhileStatement =>
				compileWhileStatement(whileStatement)
			case doWhile: IASTDoStatement =>
				compileDoWhileStatement(doWhile)
			case switch: IASTSwitchStatement =>
				compileSwitchStatement(switch)
			case x: IASTCaseStatement =>
				List(CaseLabel(x))
			case x: IASTDefaultStatement =>
				List(DefaultLabel(x))
			case _: IASTContinueStatement =>
				List(JmpLabel(continueLabelStack.head))
			case _: IASTBreakStatement =>
				List(JmpLabel(breakLabelStack.head))
			case _: IASTElaboratedTypeSpecifier =>
				List()
			case goto: IASTGotoStatement =>
				List(Goto(goto.getName.toString))
			case fcn: IASTFunctionDefinition =>
				List(fcn)
			case compound: IASTCompoundStatement =>
				compileCompoundStatement(compound)
			case decl: IASTDeclarationStatement =>
				decl.getChildren.toList.flatMap(compileNode)
			case decl: CASTSimpleDeclaration =>
				List(decl)
			case _: IASTSimpleDeclSpecifier =>
				List()
			case _: CASTTypedefNameSpecifier =>
				List()
			case decl: IASTDeclarator =>
				List(decl)
			case label: IASTLabelStatement =>
				GotoLabel(label.getName.toString) +: compileNode(label.getNestedStatement)
			case exprState: CASTExpressionStatement =>
				List(exprState.getExpression)
			case _ =>
				node +: node.getChildren.toList
	}

	private def compileIfStatement(ifStatement: IASTIfStatement)(using CEngine) = {
		val contents = compileNode(ifStatement.getThenClause)
		val elseContents = List(Option(ifStatement.getElseClause)).flatten.flatMap(compileNode)

		val jmp = if ifStatement.getElseClause != null then
			List(Jmp(elseContents.size))
		else
			List()

		val all = contents ++ jmp

		JmpIfNotEqual(ifStatement.getConditionExpression, all.size) +: (all ++ elseContents)
	}

	private def compileForStatement(forStatement: IASTForStatement)(implicit cEngine: CEngine) = {
		val breakLabel = BreakLabel()
		breakLabelStack = breakLabel +: breakLabelStack
		val continueLabel = ContinueLabel()
		continueLabelStack = continueLabel +: continueLabelStack

		val init = List(forStatement.getInitializerStatement)
		val contents = compileNode(forStatement.getBody)
		val iter = forStatement.getIterationExpression
		val beginLabel = GotoLabel("")

		breakLabelStack = breakLabelStack.tail
		continueLabelStack = continueLabelStack.tail

		val iterExpr = if iter != null then List(iter) else List()

		val jmpnz = if forStatement.getConditionExpression != null then
			List(JmpToLabelIfNotZero(forStatement.getConditionExpression, breakLabel))
		else
			List()

		val execution = List(PushVariableStack()) ++ contents ++ List(PopVariableStack(), continueLabel) ++ iterExpr
		val start = init :+ beginLabel
		val end = List(JmpLabel(beginLabel), breakLabel)

		start ++ jmpnz ++ execution ++ end
	}

	private def compileWhileStatement(whileStatement: IASTWhileStatement)(implicit cEngine: CEngine) = {
		val breakLabel = BreakLabel()
		breakLabelStack = breakLabel +: breakLabelStack
		val continueLabel = ContinueLabel()
		continueLabelStack = continueLabel +: continueLabelStack

		val contents = compileNode(whileStatement.getBody)
		val begin = GotoLabel("")
		val end = GotoLabel("")

		breakLabelStack = breakLabelStack.tail
		continueLabelStack = continueLabelStack.tail

		val body = List(JmpLabel(end), begin) ++ contents ++ List(end, continueLabel, JmpToLabelIfZero(whileStatement.getCondition, begin), breakLabel)

		PushVariableStack() +: body :+ PopVariableStack()
	}

	private def compileDoWhileStatement(doWhileStatement: IASTDoStatement)(implicit cEngine: CEngine) = {
		val breakLabel = BreakLabel()
		breakLabelStack = breakLabel +: breakLabelStack
		val continueLabel = ContinueLabel()
		continueLabelStack = continueLabel +: continueLabelStack

		val contents = compileNode(doWhileStatement.getBody)
		val begin = new Label {}

		breakLabelStack = breakLabelStack.tail
		continueLabelStack = continueLabelStack.tail

		val body = begin +: (contents ++ List(continueLabel, JmpToLabelIfZero(doWhileStatement.getCondition, begin), breakLabel))

		PushVariableStack() +: body :+ PopVariableStack()
	}

	@tailrec
	private def getParentSwitchBody(node: IASTNode): IASTStatement = node.getParent match {
		case switch: IASTSwitchStatement => switch.getBody
		case _ => getParentSwitchBody(node.getParent)
	}

	private def compileSwitchStatement(switch: IASTSwitchStatement)(implicit cEngine: CEngine) = {
		val breakLabel = BreakLabel()
		breakLabelStack = breakLabel +: breakLabelStack

		val descendants = compileNode(switch.getBody)

		val jumpTable = descendants.flatMap {
			case x @ CaseLabel(caseStatement) if switch.getBody == getParentSwitchBody(caseStatement) =>
				val cached = CachedRValue(switch.getControllerExpression)
				cached +: List(JmpToLabelIfEqual(caseStatement.getExpression, cached, x))
			case x@DefaultLabel(default) if switch.getBody == getParentSwitchBody(default) =>
				List(JmpLabel(x))
			case _ =>
				List()
		}

		breakLabelStack = breakLabelStack.tail

		val result = (jumpTable :+ JmpLabel(breakLabel)) ++ descendants :+ breakLabel

		PushVariableStack() +: result :+ PopVariableStack()
	}

	private def compileCompoundStatement(compound: IASTCompoundStatement)(using CEngine) = {
		val isTypicalCompound = compound.getParent match
			case _: (IASTSwitchStatement | CASTFunctionDefinition | CASTForStatement |
				CASTDoStatement | CASTWhileStatement) => true
			case _ => false

		if isTypicalCompound then
			compound.getStatements.flatMap(compileNode).toList
		else
			PushVariableStack() +: compound.getStatements.flatMap(compileNode).toList :+ PopVariableStack()
	}
}
