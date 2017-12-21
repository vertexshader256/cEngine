package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.op_assign
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.collection.mutable.Stack

object Declarator {

  def execute(decl: IASTDeclarator)(implicit state: State) = decl match {

    case fcnDec: IASTFunctionDeclarator => {
      val isInFunctionPrototype = !Utils.getAncestors(fcnDec).exists {
        _.isInstanceOf[IASTFunctionDefinition]
      }

      if (isInFunctionPrototype) {
        state.functionPrototypes += fcnDec
      }

      // ignore main's params for now
      val isInMain = fcnDec.getName.getRawSignature == "main"

      if (fcnDec.getName.resolveBinding().isInstanceOf[CFunction] && !fcnDec.getName.resolveBinding().asInstanceOf[CFunction].getParameters.isEmpty && !isInMain) {

        val fcn = fcnDec.getName.resolveBinding().asInstanceOf[CFunction]
        val paramDecls = new Stack[CParameter]() ++ fcn.getParameters

        var numArgs = 0

        val others = fcnDec.getChildren.filter { x => !x.isInstanceOf[IASTParameterDeclaration] && !x.isInstanceOf[IASTName] }

        if (!isInFunctionPrototype) {
          numArgs = state.context.stack.pop.asInstanceOf[RValue].value.asInstanceOf[Integer]
          val args = (0 until numArgs).map { arg => state.context.stack.pop }.reverse

          val resolvedArgs = args.map { arg => arg match {
            case StringLiteral(str) =>
              state.createStringVariable(str, false)
            case info @ LValue(_, _) => info.value
            case value @ RValue(_, _) => value
          }}

          resolvedArgs.foreach { arg =>
            if (!isInFunctionPrototype && !paramDecls.isEmpty) {
              val paramDecl = paramDecls.pop
              val newVar = state.context.addVariable(paramDecl.getName, paramDecl.getType)
              val casted = TypeHelper.cast(newVar.theType, arg.value).value
              state.Stack.writeToMemory(casted, newVar.address, newVar.theType)
            } else {
              val theType = TypeHelper.getType(arg.value)
              val sizeof = TypeHelper.sizeof(theType)
              val space = state.allocateSpace(Math.max(sizeof, 4))
              state.Stack.writeToMemory(arg.value, space, theType)
            }
          }
        }

        others
      } else {
        Seq()
      }
    }
    case arrayDecl: IASTArrayDeclarator => {
        List(Option(decl.getInitializer)).flatten.foreach{Ast.step}
        arrayDecl.getArrayModifiers.foreach{Ast.step}

        val nameBinding = decl.getName.resolveBinding()
        val name = decl.getName
        val variable = nameBinding.asInstanceOf[IVariable]
        val theType = TypeHelper.stripSyntheticTypeInfo(nameBinding.asInstanceOf[IVariable].getType)
        val dimensions = arrayDecl.getArrayModifiers.filter {
          _.getConstantExpression != null
        }.map { dim =>
          state.context.stack.pop match {
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
            val initString = state.context.stack.pop.asInstanceOf[StringLiteral].value

            val theStr = Utils.stripQuotes(initString)
            val translateLineFeed = theStr.replace("\\n", 10.asInstanceOf[Char].toString)
            val withNull = (translateLineFeed.toCharArray() :+ 0.toChar).map { char => RValue(char.toByte, new CBasicType(IBasicType.Kind.eChar, 0)) } // terminating null char

            val theArrayPtr = state.context.addVariable(name.getRawSignature, variable.getType, Seq(initString.size))
            theArrayPtr.setArray(withNull)
          } else {
            val list = initializer.getInitializerClause.asInstanceOf[IASTInitializerList]
            val size = list.getSize

            val values: Array[RValue] = (0 until size).map { x =>
              state.context.stack.pop match {
                case value@RValue(_, _) => value
                case info@LValue(_, _) => info.value
              }
            }.reverse.toArray.map{x => RValue(x.value, theType.asInstanceOf[IArrayType].getType)}

            val theArrayPtr = state.context.addVariable(name.getRawSignature, variable.getType, Array(size))
            theArrayPtr.setArray(values)
          }
          Seq()
        } else if (initializer != null) {
          val initVals: Array[Any] = (0 until initializer.getInitializerClause.getChildren.size).map { x => state.context.stack.pop }.reverse.toArray

          val initialArray = initVals.map { newInit =>
            newInit match {
              case StringLiteral(x) =>
                state.createStringVariable(newInit.asInstanceOf[StringLiteral].value, false)
              case info@LValue(_, _) => info.value
              case value@RValue(_, _) => value
            }
          }

          println("OH NO: " +variable.getType)
          val theArrayVar = state.context.addVariable(name.getRawSignature, variable.getType, dimensions)

          state.setArray(initialArray, theArrayVar.address, TypeHelper.sizeof(theArrayVar.theType))
        } else {
          println(name.getRawSignature + ":" + arrayDecl.getRawSignature)
          state.context.addVariable(name.getRawSignature, variable.getType, dimensions)
        }
        Seq()
    }
    case decl: CASTDeclarator => {
        val nameBinding = decl.getName.resolveBinding()
        val name = decl.getName

        if (nameBinding.isInstanceOf[IVariable]) {
          val theType = TypeHelper.stripSyntheticTypeInfo(nameBinding.asInstanceOf[IVariable].getType)
          val stripped = TypeHelper.stripSyntheticTypeInfo(theType)

          val variable = state.context.addVariable(name.getRawSignature, theType)

          if (!variable.isInitialized) {

            List(Option(decl.getInitializer)).flatten.foreach{x => Ast.step(x)}


            if (!stripped.isInstanceOf[CStructure]) {
              val initVal = Option(decl.getInitializer).map(x => state.context.stack.pop).getOrElse(RValue(0, null))
              BinaryExpr.parseAssign(decl, op_assign, variable, initVal)
            } else if (decl.getInitializer != null && decl.getInitializer.isInstanceOf[IASTEqualsInitializer]
              && decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause.isInstanceOf[IASTInitializerList]) {

              val clause = decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause
              val values = clause.asInstanceOf[IASTInitializerList].getClauses.map { x => state.context.stack.pop }.reverse.toList
              variable.setValues(values)
            }

            variable.isInitialized = true
          }
        }
        Seq()

    }
  }
}
