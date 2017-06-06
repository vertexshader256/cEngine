package c.engine

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import org.eclipse.cdt.internal.core.dom.parser.c._

import scala.c.engine.NodePath

object Executor {

  // 'node' must be a IASTCaseStatement or a IASTDefaultStatement
  def processSwitch(node: IASTNode): Seq[IASTNode] = {
    val codeToRun = new ListBuffer[IASTNode]()

    val siblings = node.getParent.getChildren

    var isSelfFound = false
    siblings.foreach { sib =>
      if (sib == node) {
        isSelfFound = true
      } else if (isSelfFound && !sib.isInstanceOf[IASTCaseStatement]) {
        codeToRun += sib
      }
    }

    codeToRun
  }

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
        case Stage4 =>
          val newVar = state.context.addVariable(enumerator.getName.getRawSignature, TypeHelper.pointerType)
          val value = state.stack.pop.asInstanceOf[RValue]
          state.setValue(value.value, newVar.address)
          Seq()
      }
      case enum: IASTEnumerationSpecifier => {
        case Stage4 =>
          enum.getEnumerators
      }
      case fcnDef: IASTFunctionDefinition => {
        case Stage4 =>
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
      case Stage4 =>

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

  def preload(codes: Seq[String], state: State) = {
    state.tUnit = Utils.getTranslationUnit(codes)
    state.context.pathStack.push(NodePath(state.tUnit, Stage1))

    val fcns = state.tUnit.getChildren.collect{case x:IASTFunctionDefinition => x}.filter(_.getDeclSpecifier.getStorageClass != IASTDeclSpecifier.sc_extern)
    fcns.foreach{fcnDef => state.addFunctionDef(fcnDef)}
  }

  def init(codes: Seq[String], reset: Boolean, state: State) = {
    preload(codes, state)
    run(state)

    state.context.pathStack.clear
    state.context.pathStack.push(NodePath(state.getFunction("main").node, Stage1))
  }

  def tick(state: State): Boolean = {
    val current = state.context.pathStack.headOption.getOrElse(null)
    if (current != null) {
      
      //println(current.node.getClass.getSimpleName + ":" + current.direction)

      val paths: Seq[NodePath] = if (state.isGotoing && current.direction != Stage1) {
        val result = (Executor.step(current, Gotoing)(state) orElse NoMatch)(Gotoing).map{ x => NodePath(x, Stage1)}

        if (!current.node.isInstanceOf[IASTForStatement] && !current.node.isInstanceOf[IASTDoStatement] && !current.node.isInstanceOf[IASTWhileStatement]
          && !current.node.isInstanceOf[IASTForStatement]) {
          state.context.pathStack.pop
        }

        result
      } else {

        val result = (Executor.step(current, current.direction)(state) orElse NoMatch)(current.direction).map{x => NodePath(x, Stage1)}

        current.direction match {
          case Stage1 => current.direction = Stage2
          case Stage2 => current.direction = Stage3
          case Stage3 => current.direction = Stage4
          case Stage4 => state.context.pathStack.pop
        }

        result
      }

      state.context.pathStack.pushAll(paths.reverse)

      true
    } else {
      false
    }
  }

  def run(state: State) = {
    var keepRunning = true
    while (keepRunning) {
      try {
        keepRunning = tick(state)
      } catch {
        case e =>
          throw e
      }
    }
  }
}
