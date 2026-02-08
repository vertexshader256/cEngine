package scala.c.engine

import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit

import scala.c.engine.models.Function

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.op_assign
import org.eclipse.cdt.internal.core.dom.parser.c.*

import scala.c.engine.Instructions.*
import scala.c.engine.ast.{Declarator, Expressions}
import scala.c.engine.cFunctions.*
import scala.c.engine.models.*
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait CodeRunner {
	this: CEngine =>

	private val main: Function = new Function("main", true) {
		def run(formattedOutputParams: Array[RValue], state: CEngine): Option[RValue] = None
	}

	private val program = new FunctionScope(main, null, null) {}

	def addMain(sources: List[IASTTranslationUnit]): Unit = {
		sources.foreach { tUnit =>
			tUnit.getChildren.collect { case x: IASTFunctionDefinition => x }
				.filter(fcn => fcn.getDeclSpecifier.getStorageClass != IASTDeclSpecifier.sc_extern)
				.foreach { fcnDef =>
					addFunctionDef(fcnDef, fcnDef.getDeclarator.getName.toString == "main")
				}
		}
	}

	private def pushScope(scope: FunctionScope): Unit = {
		functionContexts.push(scope)
	}

	def runCode(code: String, includePaths: Iterator[String]) = {
		val exeCode =
			s"""
					void main() {
						$code
					}
				"""

		val ast = Utils.getTranslationUnits(Seq(exeCode), includePaths.toList)
		addMain(ast)
		callTheFunction("main", null, Some(program), true)

		val theMain = functionList.find(_.name == "main").get
		functionList -= theMain
	}

	def parseGlobals(tUnits: List[IASTNode]): Unit = {
		pushScope(program)
		program.init(tUnits, this, false)
		context.run(this) // parse globals
	}
}
