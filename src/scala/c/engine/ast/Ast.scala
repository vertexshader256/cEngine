package c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

object Ast {

  val NoMatch: PartialFunction[Direction, Seq[IASTNode]] = { case _ => Seq()}

  def step(current: NodePath, direction: Direction)(implicit state: State): PartialFunction[Direction, Seq[IASTNode]] = current.node match {

    case statement: IASTStatement =>
      Statement.parse(current)
    case expression: IASTExpression =>
      Expressions.parse(expression)
    case decl: IASTDeclarator =>
      Declarator.execute(decl)
    case array: IASTArrayModifier => {
      case Stage2 =>
        if (array.getConstantExpression != null) {
          Seq(array.getConstantExpression)
        } else {
          Seq()
        }
    }
    case ptr: IASTPointer => {
      case _ => Seq()
    }
    case tUnit: IASTTranslationUnit => {
      case _ =>
        tUnit.getChildren.filterNot {
          _.isInstanceOf[IASTFunctionDefinition]
        }
    }
    case simple: IASTSimpleDeclaration => {
      case Stage2 =>
        val declSpec = simple.getDeclSpecifier
        if (declSpec.isInstanceOf[IASTEnumerationSpecifier]) {
          simple.getDeclarators :+ simple.getDeclSpecifier
        } else {
          simple.getDeclarators
        }
    }
    case enumerator: CASTEnumerator => {
      case Stage2 =>
        Seq(enumerator.getValue)
      case Exiting =>
        val newVar = state.context.addVariable(enumerator.getName, TypeHelper.pointerType)
        val value = state.context.stack.pop.asInstanceOf[RValue]
        state.Stack.writeToMemory(value.value, newVar.address, TypeHelper.pointerType)
        Seq()
    }
    case enum: IASTEnumerationSpecifier => {
      case Exiting =>
        enum.getEnumerators
    }
    case fcnDef: IASTFunctionDefinition => {
      case Exiting =>
        if (!state.context.stack.isEmpty) {
          val retVal = state.context.stack.pop
          state.context.stack.push(retVal)
        }
        Seq()
      case Stage2 =>
        Seq(fcnDef.getDeclarator, fcnDef.getBody)
    }
    case eq: IASTEqualsInitializer => {
      case Stage2 =>
        Seq(eq.getInitializerClause)
    }
    case initList: IASTInitializerList => {
      case Stage2 =>
        initList.getClauses
    }
  }
}
