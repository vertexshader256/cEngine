package scala.astViewer

import java.io.File
import java.util.HashMap

import org.eclipse.cdt.core.dom.ast.{IASTNode, _}
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.parser.{DefaultLogService, FileContent, IncludeFileContentProvider, ScannerInfo}
import scala.collection.mutable.ListBuffer

sealed trait Direction
object Entering extends Direction
object Exiting extends Direction
object Visiting extends Direction

case class Path(node: IASTNode, direction: Direction, index: Int)

object Utils {

  def parse(code: String, offset: Int): IASTCompletionNode = {
    val fileContent = FileContent.create("test", code.toCharArray)
    val log = new DefaultLogService()

    GCCLanguage.getDefault().getCompletionNode(fileContent, new ScannerInfo(), null, null, log, offset);
  }

  def findFunctions(node: IASTTranslationUnit): Seq[IASTFunctionDefinition] = {
    node.getDeclarations.collect{case decl: IASTFunctionDefinition => decl}
  }
  
  def getAncestors(node: IASTNode): Seq[IASTNode] = {
    var current = node.getParent
    val results = new ListBuffer[IASTNode]()
    while (current != null) {
      results += current
      current = current.getParent
    }
    results
  }

  def findVariable(scope: IScope, name: String, tUnit: IASTTranslationUnit): Option[IVariable] = {
    var currentScope = scope

    val scopeLookup = new IScope.ScopeLookupData(name.toCharArray, tUnit)

    while (currentScope != null && currentScope.getBindings(scopeLookup).isEmpty) {
      currentScope = currentScope.getParent
    }

    if (currentScope == null) {
      None
    } else {
      Some(currentScope.getBindings(scopeLookup).head.asInstanceOf[IVariable])
    }
  }

  def getTranslationUnit(code: String): IASTTranslationUnit = {
    val fileContent = FileContent.create("test", code.toCharArray)
    val symbolMap = new HashMap[String, String];

    val systemIncludes = List(new File(raw"C:\MinGW\include"), new File(raw"C:\MinGW\include\GL"), new File(raw"C:\MinGW\lib\gcc\mingw32\4.6.2\include"))

    val info = new ScannerInfo(symbolMap, systemIncludes.toArray.map(_.getAbsolutePath))
    val log = new DefaultLogService()
    val opts = 8
    val includes = IncludeFileContentProvider.getEmptyFilesProvider

    GCCLanguage.getDefault().getASTTranslationUnit(fileContent, info, includes, null, opts, log)
  }

  def getPath(tUnit: IASTNode): Seq[Path] = {

    var index = 0

    def getDescendants(node: IASTNode): Seq[IASTNode] = {
      node.getChildren.flatMap(x => x +: getDescendants(x))
    }

    def recurse(node: IASTNode): Seq[Path] = {
      val children = node.getChildren

      val start = Seq(Path(node, Entering, index))
      index += 1
      val result = start ++ children.flatMap { child =>
        val descendants = getDescendants(child)

        if (descendants.size == child.getChildren.size && !child.getChildren.isEmpty) {
          val node = Seq(Path(child, Visiting, index))
          index += 1
          node
        } else if (descendants.size > 1) {
          recurse(child)
        } else {
          Seq()
        }
      } ++ Seq(Path(node, Exiting, index))
      index += 1
      result
    }

    val result = recurse(tUnit)

    //result.foreach(println)

    result
  }
}
