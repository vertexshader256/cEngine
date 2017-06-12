package c.engine
package ast

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.op_assign
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.collection.mutable.Stack

object Declarator {

  def execute(decl: IASTDeclarator)(implicit state: State): PartialFunction[Direction, Seq[IASTNode]] = decl match {

    case fcnDec: IASTFunctionDeclarator => {
      case Stage2 =>
        val isInFunctionPrototype = Utils.getAncestors(fcnDec).exists {
          _.isInstanceOf[IASTSimpleDeclaration]
        }

        if (isInFunctionPrototype) {
          state.functionPrototypes += fcnDec
        }

        // ignore main's params for now
        val isInMain = fcnDec.getName.getRawSignature == "main"
        val fcnName = fcnDec.getName.getRawSignature

        val paramDecls = new Stack[IASTParameterDeclaration]() ++ fcnDec.getChildren.collect { case x: IASTParameterDeclaration => x }

        if (!paramDecls.isEmpty && !isInMain) {

          var numArgs = 0

          val others = fcnDec.getChildren.filter { x => !x.isInstanceOf[IASTParameterDeclaration] && !x.isInstanceOf[IASTName] }

          if (!isInFunctionPrototype) {
            numArgs = state.stack.pop.asInstanceOf[RValue].value.asInstanceOf[Integer]
            val args = (0 until numArgs).map { arg => state.stack.pop }.reverse

            val resolvedArgs = args.map { arg => arg match {
              case StringLiteral(str) =>
                state.createStringVariable(str, false)
              case info @ LValue(_, _) => info.value
              case value @ RValue(_, _) => value
            }}

            resolvedArgs.foreach { arg =>
              if (!isInFunctionPrototype && !paramDecls.isEmpty) {

                val paramDecl = paramDecls.pop

                val paramInfo = paramDecl.getDeclarator.getName.resolveBinding().asInstanceOf[CParameter]

                val name = paramDecl.getDeclarator.getName
                val newVar = state.context.addVariable(name, paramInfo.getType)
                val casted = TypeHelper.cast(newVar.theType, arg.value).value
                state.setValue(casted, newVar.address)
              } else {
                val theType = TypeHelper.getType(arg.value)
                val sizeof = TypeHelper.sizeof(theType)
                val space = state.allocateSpace(Math.max(sizeof, 4))
                state.setValue(arg.value, space)
              }
            }
          }

          others
        } else {
          Seq()
        }
    }
    case arrayDecl: IASTArrayDeclarator => {
      case Stage1 =>
        Seq(Option(decl.getInitializer)).flatten ++ arrayDecl.getArrayModifiers
      case Stage2 =>
        val nameBinding = decl.getName.resolveBinding()
        val name = decl.getName
        val theType = TypeHelper.stripSyntheticTypeInfo(nameBinding.asInstanceOf[IVariable].getType)
        val dimensions = arrayDecl.getArrayModifiers.filter {
          _.getConstantExpression != null
        }.map { dim =>
          state.stack.pop match {
            // can we can assume dimensions are integers
            case RValue(value, _) => value.asInstanceOf[Int]
            case info@LValue(_, _) => info.value.value.asInstanceOf[Int]
          }
        }

        val initializer = decl.getInitializer.asInstanceOf[IASTEqualsInitializer]

        // Oddly enough, it is possible to have a pointer to an array with no dimensions OR initializer:
        //    extern char *x[]

        if (dimensions.isEmpty && initializer != null) {

          if (TypeHelper.resolve(theType).getKind == IBasicType.Kind.eChar && !initializer.getInitializerClause.isInstanceOf[IASTInitializerList]) {
            // e.g. char str[] = "Hello!\n";
            val initString = state.stack.pop.asInstanceOf[StringLiteral].value

            val theStr = Utils.stripQuotes(initString)
            val translateLineFeed = theStr.replace("\\n", 10.asInstanceOf[Char].toString)
            val withNull = (translateLineFeed.toCharArray() :+ 0.toChar).map { char => RValue(char.toByte, new CBasicType(IBasicType.Kind.eChar, 0)) } // terminating null char

            val theArrayPtr = state.context.addArrayVariable(name, theType.asInstanceOf[IArrayType], Seq(initString.size))
            theArrayPtr.setArray(withNull)
          } else {
            val list = initializer.getInitializerClause.asInstanceOf[IASTInitializerList]
            val size = list.getSize

            val values: Array[RValue] = (0 until size).map { x =>
              state.stack.pop match {
                case value@RValue(_, _) => value
                case info@LValue(_, _) => info.value
              }
            }.reverse.toArray

            val theArrayPtr = state.context.addArrayVariable(name, theType.asInstanceOf[IArrayType], Array(size))
            theArrayPtr.setArray(values)
          }
          Seq()
        } else if (initializer != null) {
          val initVals: Array[Any] = (0 until initializer.getInitializerClause.getChildren.size).map { x => state.stack.pop }.reverse.toArray

          val initialArray = initVals.map { newInit =>
            newInit match {
              case StringLiteral(x) =>
                state.createStringVariable(newInit.asInstanceOf[StringLiteral].value, false)
              case info@LValue(_, _) => info.value
              case value@RValue(_, _) => value
            }
          }

          val theArrayVar = state.context.addArrayVariable(name, theType.asInstanceOf[IArrayType], dimensions)
          state.setArray(initialArray, theArrayVar.address, TypeHelper.sizeof(theArrayVar.theType))
        } else {
          state.context.addArrayVariable(name, theType.asInstanceOf[IArrayType], dimensions)
        }
        Seq()
    }
    case decl: CASTDeclarator => {
      case Stage1 => Seq(Option(decl.getInitializer)).flatten
      case Stage2 =>
        val nameBinding = decl.getName.resolveBinding()
        val name = decl.getName
        if (nameBinding.isInstanceOf[IVariable]) {
          val theType = TypeHelper.stripSyntheticTypeInfo(nameBinding.asInstanceOf[IVariable].getType)
          val stripped = TypeHelper.stripSyntheticTypeInfo(theType)

          val variable = state.context.addVariable(name, theType)

          if (!variable.isInitialized) {
            if (!stripped.isInstanceOf[CStructure]) {
              val initVal = Option(decl.getInitializer).map(x => state.stack.pop).getOrElse(RValue(0, null))
              BinaryExpr.parseAssign(decl, op_assign, variable, initVal)
            } else if (decl.getInitializer != null && decl.getInitializer.isInstanceOf[IASTEqualsInitializer]
              && decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause.isInstanceOf[IASTInitializerList]) {

              val clause = decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause
              val values = clause.asInstanceOf[IASTInitializerList].getClauses.map { x => state.stack.pop }.reverse.toList
              variable.setValues(values)
            }

            variable.isInitialized = true
          }
        }
        Seq()

    }
  }
}
