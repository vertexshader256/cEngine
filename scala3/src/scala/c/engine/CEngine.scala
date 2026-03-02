package scala.c.engine

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import scala.c.engine.models.*
import scala.collection.mutable.ListBuffer

class CEngine(val sources: List[IASTTranslationUnit], val pointerSize: NumBits) extends CodeRunner with CEngineData with Functions with Compiler {

	val memory = Memory(stackSize = 100000, dataSize = 10000, heapSize = 50000)

	val stdout = ListBuffer[Char]()

	val structs: Seq[CStructure] = sources.flatMap { src =>
		src.getDeclarations.collect { case simp: CASTSimpleDeclaration => simp.getDeclSpecifier }
			.collect { case comp: CASTCompositeTypeSpecifier => comp }
			.map { x => x.getName.resolveBinding().asInstanceOf[CStructure] }
	}

	loadFunctions()
}
