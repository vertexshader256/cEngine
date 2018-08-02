package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.op_assign
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.c.engine.ast.BinaryExpr.evaluate

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
        var numArgs = 0

        val others = fcnDec.getChildren.filter { x => !x.isInstanceOf[IASTParameterDeclaration] && !x.isInstanceOf[IASTName] }

        if (!isInFunctionPrototype) {
          numArgs = state.context.popStack.asInstanceOf[RValue].value.asInstanceOf[Integer]
          val args = (0 until numArgs).map { arg => state.context.popStack }.reverse

          val resolvedArgs = args.map { TypeHelper.resolve }

          var paramDecls = fcn.getParameters.toList

          resolvedArgs.foreach { arg =>
            if (!isInFunctionPrototype && !paramDecls.isEmpty) {
              val paramDecl = paramDecls.head
              paramDecls = paramDecls.tail
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

        val theType = nameBinding match {
          case typedef: CTypedef => TypeHelper.stripSyntheticTypeInfo(typedef)
          case vari: IVariable => TypeHelper.stripSyntheticTypeInfo(vari.getType)
        }

        //val variable = nameBinding.asInstanceOf[IVariable]
        //val theType = TypeHelper.stripSyntheticTypeInfo(nameBinding.asInstanceOf[IVariable].getType)
        val dimensions = arrayDecl.getArrayModifiers.filter {
          _.getConstantExpression != null
        }.map { dim =>
          state.context.popStack match {
            // can we can assume dimensions are integers
            case RValue(value, _) => value.asInstanceOf[Int]
            case info@LValue(_, _) => info.value.value.asInstanceOf[Int]
          }
        }

        val initializer = decl.getInitializer.asInstanceOf[IASTEqualsInitializer]

        // Oddly enough, it is possible to have a pointer to an array with no dimensions OR initializer:
        //    extern char *x[]

        if (initializer != null) {

          val (initArray, initType) = if (TypeHelper.isPointer(theType) && TypeHelper.stripSyntheticTypeInfo(TypeHelper.getPointerType(theType)).isInstanceOf[CStructure]) {

            val structType = TypeHelper.stripSyntheticTypeInfo(TypeHelper.getPointerType(theType)).asInstanceOf[CStructure]

            val structData = initializer.getInitializerClause.getChildren.flatMap { list =>
              structType.getFields.map { field => TypeHelper.resolve(state.context.popStack) }
            }.reverse

            val resultType = new CArrayType(structType)
            resultType.setModifier(new CASTArrayModifier(new CASTLiteralExpression(IASTLiteralExpression.lk_integer_constant, structData.size.toString.toCharArray)))

            (structData, resultType)
          } else if (TypeHelper.resolveBasic(theType).getKind == IBasicType.Kind.eChar && !initializer.getInitializerClause.isInstanceOf[IASTInitializerList]) {
            // e.g. char str[] = "Hello!\n";
            val initString = state.context.popStack.asInstanceOf[StringLiteral].value

            val theStr = Utils.stripQuotes(initString)
            val translateLineFeed = theStr.replace("\\n", 10.asInstanceOf[Char].toString)
            val withNull = (translateLineFeed.toCharArray() :+ 0.toChar).map { char => new RValue(char.toByte, new CBasicType(IBasicType.Kind.eChar, 0)) {} } // terminating null char

            val inferredArrayType = new CArrayType(theType.asInstanceOf[IArrayType].getType)
            inferredArrayType.setModifier(new CASTArrayModifier(new CASTLiteralExpression(IASTLiteralExpression.lk_integer_constant, initString.size.toString.toCharArray)))

            (withNull, inferredArrayType)
          } else {
            val initVals: Array[ValueType] = (0 until initializer.getInitializerClause.getChildren.size).map { x => state.context.popStack }.reverse.toArray

            val initialArray = initVals.map { TypeHelper.resolve }.map { x => new RValue(x.value, theType.asInstanceOf[IArrayType].getType) {}}

            val finalType = if (!dimensions.isEmpty) {
              theType
            } else {
              val inferredArrayType = new CArrayType(theType.asInstanceOf[IArrayType].getType)
              inferredArrayType.setModifier(new CASTArrayModifier(new CASTLiteralExpression(IASTLiteralExpression.lk_integer_constant, initialArray.size.toString.toCharArray)))
              inferredArrayType
            }

            (initialArray, finalType)
          }

          val theArrayPtr = state.context.addVariable(name.getRawSignature, initType)
          theArrayPtr.setArray(initArray)

          Seq()
        } else {
          if (!theType.asInstanceOf[CArrayType].isConst && !dimensions.isEmpty) { // an array bounded by a variable e.g x[y]

            // TODO: higher dimensions
            val inferredArrayType = new CArrayType(theType.asInstanceOf[IArrayType].getType)
            inferredArrayType.setModifier(new CASTArrayModifier(new CASTLiteralExpression(IASTLiteralExpression.lk_integer_constant, dimensions.head.toString.toCharArray)))

            state.context.addVariable(name.getRawSignature, inferredArrayType)
          } else {
            state.context.addVariable(name.getRawSignature, theType)
          }
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

            val initVals: List[ValueType] = if (!stripped.isInstanceOf[CStructure]) {
              List(Option(decl.getInitializer).map(x => state.context.popStack).getOrElse(new RValue(0, null) {}))
            } else if (decl.getInitializer != null && decl.getInitializer.isInstanceOf[IASTEqualsInitializer]
              && decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause.isInstanceOf[IASTInitializerList]) {
              val clause = decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause

              clause.asInstanceOf[IASTInitializerList].getClauses.map { x => state.context.popStack }.reverse.toList
            } else if (decl.getInitializer != null && decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause != null &&
              decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause.isInstanceOf[IASTIdExpression]) { // setting a struct equal to another struct
              val otherStruct = decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause.asInstanceOf[IASTIdExpression]

              List(state.context.resolveId(otherStruct.getName).get)
            } else {
              List()
            }

            assign(stripped, variable, initVals, op_assign)

            variable.isInitialized = true
          }
        }
        Seq()

    }
  }

  def assign(dstType: IType, dst: LValue, srcs: List[ValueType], op: Int)(implicit state: State) = {
    if (!dstType.isInstanceOf[CStructure]) {
      val result = evaluate(dst, srcs.head, op)
      val casted = TypeHelper.cast(dst.theType, result.value).value
      dst.setValue(casted)
    } else {
      if (srcs.size == 1 && srcs.head.isInstanceOf[Variable]) { // setting a struct equal to another struct
        val otherStruct = srcs.head.asInstanceOf[Variable]

        val struct = otherStruct.theType.asInstanceOf[CStructure]
        struct.getFields.foreach{ field =>
          val baseField = TypeHelper.offsetof(struct, otherStruct.address, field.getName, state)
          val theField = TypeHelper.offsetof(struct, dst.address, field.getName, state)
          theField.setValue(baseField.getValue.value)
        }
      } else {
        val struct = dstType.asInstanceOf[CStructure]
        struct.getFields.zip(srcs).foreach{ case (field, newValue) =>
          val theField = TypeHelper.offsetof(struct, dst.address, field.getName, state)
          theField.setValue(newValue.asInstanceOf[RValue].value)
        }
      }
    }
  }
}
