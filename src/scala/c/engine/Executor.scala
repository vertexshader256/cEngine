package c.engine

import org.eclipse.cdt.core.dom.ast._

import scala.c.engine.NodePath
import ast._
import ast.Ast.NoMatch
import org.eclipse.cdt.internal.core.dom.parser.c.{CASTCompositeTypeSpecifier, CASTSimpleDeclaration, CStructure}

import scala.collection.mutable.ListBuffer

object Executor {

  val declarations = new ListBuffer[CStructure]()

  def init(codes: Seq[String], state: State) = {
    val tUnit = Utils.getTranslationUnit(codes)

    val fcns = tUnit.getChildren.collect{case x:IASTFunctionDefinition => x}.filter(_.getDeclSpecifier.getStorageClass != IASTDeclSpecifier.sc_extern)
    fcns.foreach{fcnDef => state.addFunctionDef(fcnDef)}

    declarations ++= tUnit.getDeclarations.collect{case simp: CASTSimpleDeclaration => simp.getDeclSpecifier}
      .collect{case comp: CASTCompositeTypeSpecifier => comp}
      .map{x => x.getName.resolveBinding().asInstanceOf[CStructure]}

    run(tUnit, state)
  }

  def run(node: IASTNode, state: State) = {

    state.context.pathStack.push(NodePath(node, Stage1))

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

  def tick(state: State): Boolean = {
    val current = state.context.pathStack.headOption.getOrElse(null)
    if (current != null) {
      
      //println(current.node.getClass.getSimpleName + ":" + current.direction)

      val paths: Seq[NodePath] = if (state.isGotoing && current.direction != Stage1) {
        val result = (Ast.step(current, Gotoing)(state) orElse NoMatch)(Gotoing).map{ x => NodePath(x, Stage1)}

        if (state.context.node != current.node) {
          state.context.pathStack.pop
        }

        result
      } else {

        val result = (Ast.step(current, current.direction)(state) orElse NoMatch)(current.direction).map{x => NodePath(x, Stage1)}

        current.direction match {
          case Stage1 => current.direction = Stage2
          case Stage2 => current.direction = Stage3
          case Stage3 => current.direction = PreLoop
          case PreLoop => current.direction = Exiting
          case Exiting => state.context.pathStack.pop
        }

        result
      }

      state.context.pathStack.pushAll(paths.reverse)

      true
    } else {
      false
    }
  }
}
