package scala.c.engine
package ast

import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.op_assign
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.c.engine.ast.BinaryExpr.evaluate

object Declarator {

  def execute(decl: IASTDeclarator)(implicit state: State): Any = decl match {

    case fcnDec: IASTFunctionDeclarator => {
      if (Utils.getDescendants(fcnDec).exists{x => x.isInstanceOf[IASTEqualsInitializer]}) {
        // when you're initializing a function pointer: int (*funcPtr2)(int, int) = blah2;

        val nameBinding = fcnDec.getNestedDeclarator.getName.resolveBinding()
        val name = fcnDec.getNestedDeclarator.getName

        if (nameBinding.isInstanceOf[IVariable]) {
          val theType = TypeHelper.stripSyntheticTypeInfo(nameBinding.asInstanceOf[IVariable].getType)
          val variable = state.context.addVariable(name.toString, theType)
          Ast.step(fcnDec.getInitializer)
          variable.setValue(TypeHelper.resolve(state.context.popStack))
        }
      } else {

        val isInFunctionPrototype = !Utils.getAncestors(fcnDec).exists {
          _.isInstanceOf[IASTFunctionDefinition]
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
    }
    case arrayDecl: IASTArrayDeclarator => {

        val initializer = decl.getInitializer.asInstanceOf[IASTEqualsInitializer]

        val name = if (arrayDecl.getNestedDeclarator != null) {
          arrayDecl.getNestedDeclarator.getName
        } else {
          arrayDecl.getName
        }

        val theType = TypeHelper.getBindingType(name.resolveBinding())

        // Oddly enough, it is possible to have a pointer to an array with no dimensions OR initializer:
        //    extern char *x[]

        if (decl.getInitializer != null) {

          val equals = decl.getInitializer.asInstanceOf[IASTEqualsInitializer]
          val hasList = equals.getInitializerClause.isInstanceOf[IASTInitializerList]

          def flattenInitList(node: IASTInitializerClause): List[ValueType] = node match {
            case list: IASTInitializerList =>
              list.getClauses.toList.flatMap(flattenInitList)
            case lit: IASTLiteralExpression =>
              List(Expressions.evaluate(lit).get)
            case unary: IASTUnaryExpression =>
              List(Expressions.evaluate(unary).get)
            case id: IASTIdExpression =>
              val variable = Expressions.evaluate(id).get.asInstanceOf[Variable]
              List(variable.rValue)
            case bin: IASTBinaryExpression =>
              Expressions.evaluate(bin).get match {
                case variable: Variable => List(variable.rValue)
                case rVal: RValue => List(rVal)
              }
            case x => println("ERROR FLATTEN INIT LIST"); println(x.getClass.getSimpleName); null;
          }

          if (TypeHelper.isPointerOrArray(theType) && TypeHelper.getPointerType(theType).isInstanceOf[CStructure]) {
            val data =  List(Option(decl.getInitializer)).flatten
            data.foreach{Ast.step}
            val structType = TypeHelper.getPointerType(theType).asInstanceOf[CStructure]

            val structData = initializer.getInitializerClause.getChildren.flatMap { list =>
              structType.getFields.map { field => TypeHelper.resolve(state.context.popStack) }
            }.reverse.toList

            state.context.addArrayVariable(name.toString, theType, structData)
          } else if (TypeHelper.resolveBasic(theType).getKind == IBasicType.Kind.eChar && !initializer.getInitializerClause.isInstanceOf[IASTInitializerList]) {
            // e.g. char str[] = "Hello!\n";
            List(Option(decl.getInitializer)).flatten.foreach{Ast.step}
            val initString = state.context.popStack.asInstanceOf[StringLiteral].value
            state.createStringArrayVariable(name.toString, initString)
          } else if (hasList) { // e.g '= {1,2,3,4,5}' or x[2][2] = {{1,2},{3,4},{5,6},{7,8}}

            val initialArray = flattenInitList(equals.getInitializerClause).map(TypeHelper.resolve)

            if (initialArray.size == 1 && initialArray.head.value.isInstanceOf[Int] &&
              initialArray.head.value.asInstanceOf[Int] == 0) { // e.g = {0}
              val newVar = state.context.addArrayVariable(name.toString, theType, initialArray)
              val zeroArray = (0 until newVar.sizeof).map{x => 0.toByte}.toArray
              state.writeDataBlock(zeroArray, newVar.address)
            } else {
              val newArray = if (!theType.asInstanceOf[CArrayType].getType.isInstanceOf[CPointerType]) {
                val baseType = TypeHelper.resolveBasic(theType)
                initialArray.map { x => TypeHelper.cast(baseType, x.value)}
              } else {
                initialArray
              }

              state.context.addArrayVariable(name.toString, theType, newArray)
            }
          } else { // initializing array to address, e.g int (*ptr)[5] = &x[1];
            Ast.step(decl.getInitializer)
            val initVal = TypeHelper.resolve(state.context.popStack)
            val newArray = List(initVal)
            state.context.addArrayVariable(name.toString, theType, newArray)
          }

          Seq()
        } else { // no initializer

          val dimensions = arrayDecl.getArrayModifiers.toList.filter {
            _.getConstantExpression != null
          }.map { _ =>
            arrayDecl.getArrayModifiers.foreach{Ast.step}
            val value = TypeHelper.resolve(state.context.popStack).value
            TypeHelper.cast(TypeHelper.intType, value).value.asInstanceOf[Int]
          }

          val aType = if (theType.isInstanceOf[CArrayType] && !theType.asInstanceOf[CArrayType].isConst && !dimensions.isEmpty) { // an array bounded by a variable e.g x[y]
              def createdSizedArrayType(theType: CArrayType, dimensions: List[Int]): CArrayType = {
                val arrayType = if (theType.getType.isInstanceOf[CArrayType]) {
                  new CArrayType(createdSizedArrayType(theType.getType.asInstanceOf[CArrayType], dimensions.tail))
                } else {
                  new CArrayType(theType.getType)
                }

                arrayType.setModifier(new CASTArrayModifier(new CASTLiteralExpression(IASTLiteralExpression.lk_integer_constant, dimensions.head.toString.toCharArray)))
                arrayType
              }

              createdSizedArrayType(theType.asInstanceOf[CArrayType], dimensions.reverse)
          } else {
            theType
          }

          state.context.addVariable(name.toString, aType)
        }
        Seq()
    }
    case decl: CASTDeclarator => {
        val nameBinding = decl.getName.resolveBinding()
        val name = decl.getName

        if (nameBinding.isInstanceOf[IVariable]) {
          val theType = TypeHelper.stripSyntheticTypeInfo(nameBinding.asInstanceOf[IVariable].getType)

          val variable = if (nameBinding.asInstanceOf[IVariable].isExtern) {
            state.context.addExternVariable(name.toString, theType)
          } else {
            state.context.addVariable(name.toString, theType)
          }

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
      val result = Expressions.evaluate(decl).get

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
              Expressions.evaluate(init.getOperand).get
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
      Ast.step(decl)
      state.context.popStack
      List()
    } else {
      List()
    }
  }

  def assign(dst: LValue, srcs: List[ValueType], equals: IASTInitializerClause, op: Int)(implicit state: State): LValue = {
    if (!dst.theType.isInstanceOf[CStructure]) {
      val result = evaluate(dst, srcs.head, op) match {
        case file @ FileRValue(_) => file
        case x => TypeHelper.cast(dst.theType, x.value)
      }
      dst.setValue(result)
    } else if (equals.isInstanceOf[IASTFunctionCallExpression]) {
      state.copy(dst.address, state.Stack.insertIndex - dst.sizeof, dst.sizeof)
    } else if (equals.isInstanceOf[IASTTypeIdInitializerExpression]) {
      val otherStruct = Expressions.evaluate(equals).get.asInstanceOf[LValue]
      state.copy(dst.address, otherStruct.address, dst.sizeof)
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

    dst
  }
}
