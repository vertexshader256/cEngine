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

      if (fcnDec.getName.resolveBinding().isInstanceOf[CFunction] && !fcnDec.getName.resolveBinding().asInstanceOf[CFunction].getParameters.isEmpty) {

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

        val name = if (arrayDecl.getNestedDeclarator != null) {
          arrayDecl.getNestedDeclarator.getName
        } else {
          arrayDecl.getName
        }

        val theType = TypeHelper.getBindingType(name.resolveBinding())

        val dimensions = arrayDecl.getArrayModifiers.toList.filter {
          _.getConstantExpression != null
        }.map { _ =>
          TypeHelper.resolve(state.context.popStack).value.asInstanceOf[Int]
        }

        val initializer = decl.getInitializer.asInstanceOf[IASTEqualsInitializer]

        // Oddly enough, it is possible to have a pointer to an array with no dimensions OR initializer:
        //    extern char *x[]

        if (initializer != null) {

          if (TypeHelper.isPointer(theType) && TypeHelper.getPointerType(theType).isInstanceOf[CStructure]) {

            val structType = TypeHelper.getPointerType(theType).asInstanceOf[CStructure]

            val structData = initializer.getInitializerClause.getChildren.flatMap { list =>
              structType.getFields.map { field => TypeHelper.resolve(state.context.popStack) }
            }.reverse.toList

            val resultType = new CArrayType(structType)
            resultType.setModifier(new CASTArrayModifier(new CASTLiteralExpression(IASTLiteralExpression.lk_integer_constant, structData.size.toString.toCharArray)))

            state.context.addArrayVariable(name.toString, resultType, structData)
          } else if (TypeHelper.resolveBasic(theType).getKind == IBasicType.Kind.eChar && !initializer.getInitializerClause.isInstanceOf[IASTInitializerList]) {
            // e.g. char str[] = "Hello!\n";
            val initString = state.context.popStack.asInstanceOf[StringLiteral].value
            state.createStringArrayVariable(name.toString, initString)
          } else { // e.g '= {1,2,3,4,5}' or '= variable'
            val initVals: Array[ValueType] = (0 until initializer.getInitializerClause.getChildren.size).map { x => state.context.popStack }.reverse.toArray

            if (initVals.size > 1) {
              val baseType = TypeHelper.resolveBasic(theType)
              val initialArray = initVals.map { x => TypeHelper.cast(baseType, TypeHelper.resolve(x).value)}.toList

              val finalType = if (!dimensions.isEmpty) {
                theType
              } else {
                val inferredArrayType = new CArrayType(theType.asInstanceOf[IArrayType].getType)
                inferredArrayType.setModifier(new CASTArrayModifier(new CASTLiteralExpression(IASTLiteralExpression.lk_integer_constant, initialArray.size.toString.toCharArray)))
                inferredArrayType
              }

              state.context.addArrayVariable(name.toString, finalType, initialArray)
            } else if (initVals.head.isInstanceOf[RValue]) {
              val rValue = initVals.head.asInstanceOf[RValue]
              state.context.addArrayVariable(name.toString, theType, List(rValue))
            } else {
              val lValue = initVals.head.asInstanceOf[LValue]
              val theArrayPtr = state.context.addVariable(name.toString, theType)
              state.copy(theArrayPtr.address, lValue.address, lValue.sizeof)
            }
          }

          Seq()
        } else {
          if (!theType.asInstanceOf[CArrayType].isConst && !dimensions.isEmpty) { // an array bounded by a variable e.g x[y]
            // TODO: higher dimensions
            val inferredArrayType = new CArrayType(theType.asInstanceOf[IArrayType].getType)
            inferredArrayType.setModifier(new CASTArrayModifier(new CASTLiteralExpression(IASTLiteralExpression.lk_integer_constant, dimensions.head.toString.toCharArray)))

            state.context.addVariable(name.toString, inferredArrayType)
          } else {
            state.context.addVariable(name.toString, theType)
          }
        }
        Seq()
    }
    case decl: CASTDeclarator => {
        val nameBinding = decl.getName.resolveBinding()
        val name = decl.getName

        if (nameBinding.isInstanceOf[IVariable]) {
          val theType = TypeHelper.stripSyntheticTypeInfo(nameBinding.asInstanceOf[IVariable].getType)

          val variable = state.context.addVariable(name.toString, theType)

          if (!variable.isInitialized) {

            if (decl.getInitializer.isInstanceOf[IASTEqualsInitializer]) {
              val initClause = decl.getInitializer.asInstanceOf[IASTEqualsInitializer].getInitializerClause
              val initVals = getRValues(initClause, theType)
              assign(variable, initVals, initClause, op_assign)
            }

            variable.isInitialized = true
          }
        }
        Seq()
    }
  }

  def getRValues(decl: IASTInitializerClause, theType: IType)(implicit state: State): List[ValueType] = {
    if (!theType.isInstanceOf[CStructure]) {
      val result = if (decl != null) {
        Ast.step(decl)
        state.context.popStack
      } else {
        RValue(0, null)
      }

      List(result)
    } else if (decl != null && decl.isInstanceOf[IASTInitializerList]) {
      val clause = decl
      val result = if (decl != null) {

        if (Utils.getDescendants(decl).exists{node => node.isInstanceOf[CASTDesignatedInitializer]}) {
          // {.y = 343, .x = 543, .next = 8578}

          val initializers = Utils.getDescendants(decl).collect{case des: CASTDesignatedInitializer => des}
          val struct = theType.asInstanceOf[CStructure]

          struct.getFields.map{ field =>
            initializers.find{ init =>
              val fieldName = init.getDesignators.toList.head.asInstanceOf[CASTFieldDesignator].getName.toString
              fieldName == field.getName
            }.map{ init =>
              Ast.step(init.getOperand)
              state.context.popStack
            }.getOrElse{
              TypeHelper.zero
            }
          }.toList
        } else {
          Ast.step(decl)
          clause.asInstanceOf[IASTInitializerList].getClauses.map { x => state.context.popStack }.reverse.toList
        }
      } else {
        List(RValue(0, null))
      }

      result
    } else if (decl != null && decl.isInstanceOf[IASTIdExpression]) { // setting a struct equal to another struct
      val otherStruct = decl.asInstanceOf[IASTIdExpression]
      List(state.context.resolveId(otherStruct.getName).get)
    } else if (decl != null && decl.isInstanceOf[IASTFunctionCallExpression]) { // setting a struct equal to function return
      val struct = theType.asInstanceOf[CStructure]
      Ast.step(decl)
      state.context.popStack
      List()
    } else if (decl.isInstanceOf[IASTFunctionCallExpression]) {
      List()
    } else {
      List()
    }
  }

  def assign(dst: LValue, srcs: List[ValueType], equals: IASTInitializerClause, op: Int)(implicit state: State): LValue = {
    if (!dst.theType.isInstanceOf[CStructure]) {
      val result = evaluate(dst, srcs.head, op)
      val casted = TypeHelper.cast(dst.theType, result.value).value
      dst.setValue(casted)
    } else {

      if (equals.isInstanceOf[IASTFunctionCallExpression]) {
        state.copy(dst.address, state.Stack.insertIndex - dst.sizeof, dst.sizeof)
      } else if (equals.isInstanceOf[IASTExpression]) { // setting a struct equal to another struct
        val otherStruct = srcs.head.asInstanceOf[LValue]
        state.copy(dst.address, otherStruct.address, dst.sizeof)
      } else { // e.g struct Test test = {1.0, 2, "three"}
        val struct = dst.theType.asInstanceOf[CStructure]
        struct.getFields.zip(srcs).foreach{ case (field, newValue) =>
          val theField = TypeHelper.offsetof(struct, dst.address, field.getName, state)
          assign(theField, List(newValue), equals, op)
        }
      }
    }

    dst
  }
}
