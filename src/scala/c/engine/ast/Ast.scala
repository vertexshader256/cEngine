package c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

object Ast {

  val NoMatch: PartialFunction[Direction, Seq[IASTNode]] = { case _ => Seq()}

  def step(current: Any)(implicit state: State) = current match {

    case statement: IASTStatement =>
      println("PARSING STATEMENT")
      Statement.parse(statement)
    case expression: IASTExpression => {
        state.context.stack.pushAll(Expressions.evaluate(expression))
        Seq()
    }
    case decl: IASTDeclarator =>
      println("PARSING DECL")
      Declarator.execute(decl)
    case array: IASTArrayModifier => {
      if (array.getConstantExpression != null) {
        Seq(array.getConstantExpression)
      } else {
        Seq()
      }
    }
    case JmpIfNotEqual(expr, lines) =>
      val raw = Expressions.evaluate(expr).get
      val result = TypeHelper.resolveBoolean(raw)
      println("JMPNEQ RESULT: " + raw)
      if (!result) {
        state.pathIndex += lines
      }
    case JmpToLabelIfNotEqual(expr, label) =>
      val raw = Expressions.evaluate(expr).get
      val result = TypeHelper.resolveBoolean(raw)
      println("JMP NEQ LABEL RESULT: " + raw)
      if (!result) {
        state.pathIndex = label.address
      }
    case JmpLabel(label) =>
      state.pathIndex = label.address
    case JmpToLabelIfEqual(expr, label) =>
      val raw = Expressions.evaluate(expr).get
      val result = TypeHelper.resolveBoolean(raw)
      println("JMP EQ LABEL RESULT: " + raw)
      if (result) {
        state.pathIndex = label.address
      }
    case Jmp(lines) =>
      println("JUMPING: " + lines)
      state.pathIndex += lines
    case ptr: IASTPointer => {
      Seq()
    }
    case tUnit: IASTTranslationUnit => {
      tUnit.getChildren.filterNot {
        _.isInstanceOf[IASTFunctionDefinition]
      }
    }
    case simple: IASTSimpleDeclaration => {
      val declSpec = simple.getDeclSpecifier
      if (declSpec.isInstanceOf[IASTEnumerationSpecifier]) {
        simple.getDeclarators :+ simple.getDeclSpecifier
      } else {
        simple.getDeclarators
      }
    }
//    case enumerator: CASTEnumerator => {
//      case Stage2 =>
//        Seq(enumerator.getValue)
//      case Exiting =>
//        val newVar = state.context.addVariable(enumerator.getName, TypeHelper.pointerType)
//        val value = state.context.stack.pop.asInstanceOf[RValue]
//        state.Stack.writeToMemory(value.value, newVar.address, TypeHelper.pointerType)
//        Seq()
//    }
//    case enum: IASTEnumerationSpecifier => {
//      case Exiting =>
//        enum.getEnumerators
//    }
    case fcnDef: IASTFunctionDefinition => {
//        if (!state.context.stack.isEmpty) {
//          val retVal = state.context.stack.pop
//          state.context.stack.push(retVal)
//        }
//        Seq()
//      case Stage2 =>
        Seq(fcnDef.getDeclarator, fcnDef.getBody)
    }
    case initList: IASTInitializerList => {
        initList.getClauses
    }
    case label: Label =>
  }
}
