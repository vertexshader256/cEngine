package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

object Ast {

  def executeCustomInstructions(current: Any)(implicit state: State): Unit = current match {
    case PushVariableStack() =>
      state.context.pushVariableScope
    case PopVariableStack() =>
      state.context.popVariableScope
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
    case JmpToLabelIfEqual(expr1, expr2, label) =>
      val raw1 = TypeHelper.resolve(Expressions.evaluate(expr1).get).value
      val raw2 = TypeHelper.resolve(Expressions.evaluate(expr2).get).value
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
          if (!isWithinFunction) step(fcn) else step(fcn.getNestedDeclarator)
        case x => step(x)
      }

      if (declSpec.isInstanceOf[IASTEnumerationSpecifier]) {
        step(simple.getDeclSpecifier)
      }
    }
    case enumerator: CASTEnumerator => {
      step(enumerator.getValue)

      val newVar = state.context.addVariable(enumerator.getName.toString, TypeHelper.intType)
      val value = state.context.popStack.asInstanceOf[RValue]
      state.Stack.writeToMemory(value.value, newVar.address, TypeHelper.intType)
    }
    case enum: IASTEnumerationSpecifier => {
      enum.getEnumerators.foreach(step)
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
