package scala.c.engine

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.op_assign
import org.eclipse.cdt.internal.core.dom.parser.c.*

import scala.c.engine.Instructions.*
import scala.c.engine.ast.{Declarator, Expressions}
import scala.c.engine.cFunctions.*
import scala.c.engine.models.*
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class State(val sources: List[IASTTranslationUnit], val pointerSize: NumBits) extends CodeRunner with CEngineData with Functions {

	val stack = Memory(stackSize = 100000, dataSize = 10000, heapSize = 50000)

	var varArgStartingAddr = List[Int]()

	val stdout = ListBuffer[Char]()

	var breakLabelStack = List[Label]()
	var continueLabelStack = List[Label]()

	val structs: Seq[CStructure] = sources.flatMap { src =>
		src.getDeclarations.collect { case simp: CASTSimpleDeclaration => simp.getDeclSpecifier }
			.collect { case comp: CASTCompositeTypeSpecifier => comp }
			.map { x => x.getName.resolveBinding().asInstanceOf[CStructure] }
	}

	// ************************************************* //
	//                  Constructor                      //
	// ************************************************* //

	loadFunctions()
}
