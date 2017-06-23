package c.engine

import org.eclipse.cdt.core.dom.ast._

import scala.c.engine.NodePath
import ast._
import ast.Ast.NoMatch

object Executor {

  def preload(codes: Seq[String], state: State) = {
    state.tUnit = Utils.getTranslationUnit(codes)
    state.context.pathStack.push(NodePath(state.tUnit, Stage1))

    val fcns = state.tUnit.getChildren.collect{case x:IASTFunctionDefinition => x}.filter(_.getDeclSpecifier.getStorageClass != IASTDeclSpecifier.sc_extern)
    fcns.foreach{fcnDef => state.addFunctionDef(fcnDef)}

    run(state)
    state.context.pathStack.clear
  }

  def init(codes: Seq[String], reset: Boolean, state: State) = {
    preload(codes, state)


    state.context.pathStack.push(NodePath(state.getFunction("main").node, Stage1))
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

  def tick(state: State): Boolean = {
    val current = state.context.pathStack.headOption.getOrElse(null)
    if (current != null) {
      
      //println(current.node.getClass.getSimpleName + ":" + current.direction)

      val paths: Seq[NodePath] = if (state.isGotoing && current.direction != Stage1) {
        val result = (Ast.step(current, Gotoing)(state) orElse NoMatch)(Gotoing).map{ x => NodePath(x, Stage1)}

        if (!current.node.isInstanceOf[IASTForStatement] && !current.node.isInstanceOf[IASTDoStatement] && !current.node.isInstanceOf[IASTWhileStatement]
          && !current.node.isInstanceOf[IASTForStatement]) {
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
