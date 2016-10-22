package app.astViewer

import java.io.File
import java.util.HashMap

import org.eclipse.cdt.core.dom.ast.{IASTNode, _}
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.parser.{DefaultLogService, FileContent, IncludeFileContentProvider, ScannerInfo}
import scala.collection.mutable.ListBuffer
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._

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
  
  def stripQuotes(str: String): String = {
    str.tail.reverse.tail.reverse
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
  
  def isAssignment(op: Int) = {
      op == op_assign ||
      op == op_plusAssign ||
      op == op_minusAssign ||
      op == op_binaryXorAssign ||
      op == op_multiplyAssign ||
      op == op_divideAssign
    }
  
  def getDescendants(node: IASTNode): Seq[IASTNode] = {
    Seq(node) ++ node.getChildren.flatMap{x => getDescendants(x)}
  }

  def getTranslationUnit(code: String): IASTTranslationUnit = {
    val preprocess = Gcc.preprocess(code)
    val fileContent = FileContent.create("test", preprocess.toCharArray)
    val symbolMap = new HashMap[String, String];

    val systemIncludes = List(new File(raw"C:\MinGW\include"), new File(raw"C:\MinGW\include\GL"), new File(raw"C:\MinGW\lib\gcc\mingw32\4.6.2\include"))

    val info = new ScannerInfo(symbolMap, systemIncludes.toArray.map(_.getAbsolutePath))
    val log = new DefaultLogService()
    val opts = 8
    val includes = IncludeFileContentProvider.getEmptyFilesProvider

    val tUnit = GCCLanguage.getDefault().getASTTranslationUnit(fileContent, info, includes, null, opts, log)
    tUnit
  }
}
