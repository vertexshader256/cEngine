package c.engine
package ast

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.c.engine.NodePath
import scala.collection.mutable.ListBuffer

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
        val value = state.stack.pop.asInstanceOf[RValue]
        state.setValue(value.value, newVar.address)
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
          if (fcnDef.getDeclarator.getName.getRawSignature != "main") {
            state.popFunctionContext
          }
          state.context.stack.push(retVal)
        } else {
          if (fcnDef.getDeclarator.getName.getRawSignature != "main") {
            state.popFunctionContext
          }
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
    case typeId: IASTTypeId => {
      case Exiting =>

        val result: TypeInfo = typeId.getDeclSpecifier match {
          case simple: IASTSimpleDeclSpecifier =>
            import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclSpecifier._

            var config = 0

            if (simple.isLong) {
              config |= IBasicType.IS_LONG
            }

            if (simple.isUnsigned) {
              config |= IBasicType.IS_UNSIGNED
            } else {
              config |= IBasicType.IS_SIGNED
            }

            var result: IType = simple.getType match {
              case `t_unspecified` => new CBasicType(IBasicType.Kind.eInt, config)
              case `t_int`    => new CBasicType(IBasicType.Kind.eInt, config)
              case `t_float`  => new CBasicType(IBasicType.Kind.eFloat, config)
              case `t_double` => new CBasicType(IBasicType.Kind.eDouble, config)
              case `t_char`   => new CBasicType(IBasicType.Kind.eChar, config)
              case `t_void`   => new CBasicType(IBasicType.Kind.eVoid, config)
            }

            for (ptr <- typeId.getAbstractDeclarator.getPointerOperators) {
              result = new CPointerType(result, 0)
            }

            TypeInfo(result)

          case typespec: CASTTypedefNameSpecifier =>
            TypeInfo(typespec.getName.resolveBinding().asInstanceOf[IType])
          case elab: CASTElaboratedTypeSpecifier =>
            TypeInfo(elab.getName.resolveBinding().asInstanceOf[CStructure])
        }

        state.stack.push(result)
        Seq()
    }
  }
}
