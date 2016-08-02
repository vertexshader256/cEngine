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

case class Path(node: IASTNode, direction: Direction)

object Utils {

  def parse(code: String, offset: Int): IASTCompletionNode = {
    val fileContent = FileContent.create("test", code.toCharArray)
    val log = new DefaultLogService()

    GCCLanguage.getDefault().getCompletionNode(fileContent, new ScannerInfo(), null, null, log, offset);
  }

  def findFunctions(node: IASTTranslationUnit): Seq[IASTFunctionDefinition] = {
    node.getDeclarations.collect{case decl: IASTFunctionDefinition => decl}
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

  def getPath(tUnit: IASTTranslationUnit): Seq[Path] = {

    def getDescendants(node: IASTNode): Seq[IASTNode] = {
      node.getChildren.flatMap(x => x +: getDescendants(x))
    }

    def recurse(node: IASTNode): Seq[Path] = {
      val children = node.getChildren
      Seq(Path(node, Entering)) ++ children.flatMap { child =>
        val descendants = getDescendants(child)

        if (descendants.size == child.getChildren.size && !child.getChildren.isEmpty) {
          Seq(Path(child, Visiting))
        } else if (descendants.size > 1) {
          recurse(child)
        } else {
          Seq()
        }
      }++ Seq(Path(node, Exiting))
    }

    val result = recurse(tUnit)

    result.foreach(println)

    result
  }
}
