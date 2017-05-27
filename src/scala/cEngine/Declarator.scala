package cEngine

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.op_assign
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.{CASTDeclarator, CBasicType, CStructure, CTypedef}

object Declarator {

  def execute(decl: IASTDeclarator, direction: Direction)(implicit state: State): Seq[IASTNode] = {
    if (direction == Entering) {
      decl match {
        case array: IASTArrayDeclarator =>
          Seq(Option(decl.getInitializer)).flatten ++ array.getArrayModifiers
        case _ =>
          Seq(Option(decl.getInitializer)).flatten
      }
    } else {
      val nameBinding = decl.getName.resolveBinding()

      if (nameBinding.isInstanceOf[IVariable]) {
        val theType = TypeHelper.stripSyntheticTypeInfo(nameBinding.asInstanceOf[IVariable].getType)

        val name = decl.getName.getRawSignature

        decl match {
          case arrayDecl: IASTArrayDeclarator =>

            val dimensions = arrayDecl.getArrayModifiers.filter{_.getConstantExpression != null}.map{dim => state.stack.pop match {
              // can we can assume dimensions are integers
              case RValue(value, _) => value.asInstanceOf[Int]
              case info @ LValue(_, _) => info.value.value.asInstanceOf[Int]
            }}

            val initializer = decl.getInitializer.asInstanceOf[IASTEqualsInitializer]

            // Oddly enough, it is possible to have a pointer to an array with no dimensions OR initializer:
            //    extern char *x[]

            if (dimensions.isEmpty && initializer != null) {

              if (TypeHelper.resolve(theType).getKind == IBasicType.Kind.eChar && !initializer.getInitializerClause.isInstanceOf[IASTInitializerList]) {
                // e.g. char str[] = "Hello!\n";
                val initString = state.stack.pop.asInstanceOf[StringLiteral].value

                val theStr = Utils.stripQuotes(initString)
                val translateLineFeed = theStr.replace("\\n", 10.asInstanceOf[Char].toString)
                val withNull = (translateLineFeed.toCharArray() :+ 0.toChar).map{char => RValue(char.toByte, new CBasicType(IBasicType.Kind.eChar, 0))} // terminating null char

                val theArrayPtr =  state.context.addArrayVariable(name, theType.asInstanceOf[IArrayType], Seq(initString.size))
                theArrayPtr.setArray(withNull)
              } else {
                val list = initializer.getInitializerClause.asInstanceOf[IASTInitializerList]
                val size = list.getSize

                val values: Array[RValue] = (0 until size).map{ x => state.stack.pop match {
                  case value @ RValue(_,_) => value
                  case info @ LValue(_,_) => info.value
                }}.reverse.toArray

                val theArrayPtr = state.context.addArrayVariable(name, theType.asInstanceOf[IArrayType], Array(size))
                theArrayPtr.setArray(values)
              }
            } else if (initializer != null) {
              val initVals: Array[Any] = (0 until initializer.getInitializerClause.getChildren.size).map{x => state.stack.pop}.reverse.toArray

              val initialArray = initVals.map { newInit =>
                newInit match {
                  case StringLiteral(x) =>
                    state.createStringVariable(newInit.asInstanceOf[StringLiteral].value, false)
                  case info @ LValue(_, _) => info.value
                  case value @ RValue(_, _) => value
                }
              }

              val theArrayVar = state.context.addArrayVariable(name, theType.asInstanceOf[IArrayType], dimensions)
              state.setArray(initialArray, theArrayVar.address, TypeHelper.sizeof(theArrayVar.theType))
            } else {
              state.context.addArrayVariable(name, theType.asInstanceOf[IArrayType], dimensions)
            }
          case decl: CASTDeclarator =>

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
      } else {
        Seq()
      }
    }
  }
}
