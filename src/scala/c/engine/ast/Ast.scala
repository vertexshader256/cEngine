package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

object Ast {

  val NoMatch: PartialFunction[Direction, Seq[IASTNode]] = { case _ => Seq()}

  def step(current: Any)(implicit state: State): Unit = current match {

    case statement: IASTStatement =>
      Statement.parse(statement)
    case expression: IASTExpression => {
        state.context.stack.pushAll(Expressions.evaluate(expression))
        Seq()
    }
    case decl: IASTDeclarator =>
      Declarator.execute(decl)
    case array: IASTArrayModifier => {
      List(Option(array.getConstantExpression)).flatten.foreach(step)
    }
    case JmpIfNotEqual(expr, lines) =>
      val raw = Expressions.evaluate(expr).get
      val result = TypeHelper.resolveBoolean(raw)
      if (!result) {
        state.context.pathIndex += lines
      }
    case JmpToLabelIfNotZero(expr, label) =>
      val raw = Expressions.evaluate(expr).get
      val result = TypeHelper.resolveBoolean(raw)
      if (!result) {
        state.context.pathIndex = label.address
      }
    case JmpLabel(label) =>
      state.context.pathIndex = label.address
    case JmpToLabelIfZero(expr, label) =>
      val raw = Expressions.evaluate(expr).get
      val result = TypeHelper.resolveBoolean(raw)
      if (result) {
        state.context.pathIndex = label.address
      }
    case JmpToLabelIfNotEqual(expr1, expr2, label) =>
      val raw1 = TypeHelper.resolve(Expressions.evaluate(expr1).get).value
      val raw2 = TypeHelper.resolve(Expressions.evaluate(expr2).get).value
      if (raw1 != raw2) {
        state.context.pathIndex = label.address
      }
    case JmpToLabelIfEqual(expr1, expr2, label) =>
      val raw1 = TypeHelper.resolve(Expressions.evaluate(expr1).get).value
      val raw2 = TypeHelper.resolve(Expressions.evaluate(expr2).get).value
      if (raw1 == raw2) {
        state.context.pathIndex = label.address
      }
    case Jmp(lines) =>
      state.context.pathIndex += lines
    case jmp: JmpName =>
      state.context.pathIndex = jmp.destAddress
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

      val isWithinFunction = Utils.getAncestors(simple).exists{_.isInstanceOf[IASTFunctionDefinition]}

      simple.getDeclarators.foreach{
        case fcn: IASTFunctionDeclarator =>
          if (!isWithinFunction) step(fcn) else step(fcn.getNestedDeclarator)
        case x => step(x)
      }

      if (declSpec.isInstanceOf[IASTEnumerationSpecifier]) {
        step(simple.getDeclSpecifier)
      }
    }
    case enumerator: CASTEnumerator => {
      if (enumerator.getValue != null) {
        step(enumerator.getValue)
      } else {
        state.context.stack.push(new RValue(enumerator.getParent.getChildren.indexOf(enumerator) - 1, TypeHelper.pointerType))
      }
      val variable = new CVariable(enumerator.getName) {
        override def getType = TypeHelper.pointerType
      }

      val newVar = state.context.addVariable(enumerator.getName.getRawSignature, variable)
      val value = state.context.stack.pop.asInstanceOf[RValue]
      state.Stack.writeToMemory(value.value, newVar.address, TypeHelper.pointerType)
    }
    case enum: IASTEnumerationSpecifier => {
      enum.getEnumerators.foreach(step)
    }
    case fcnDef: IASTFunctionDefinition => {
//        if (!state.context.stack.isEmpty) {
//          val retVal = state.context.stack.pop
//          state.context.stack.push(retVal)
//        }
//        Seq()
//      case Stage2 =>
//      step(fcnDef.getDeclarator)
//      step(fcnDef.getBody)
    }
    case initList: IASTInitializerList => {
      initList.getClauses.foreach{step}
    }
    case equals: IASTEqualsInitializer =>
      step(equals.getInitializerClause)
    case label: Label =>
    case GotoLabel =>
  }
}
