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
      op == op_divideAssign ||
      op == op_shiftRightAssign
    }

  def isNestedPointer(theType: IType) = {
    theType.isInstanceOf[IPointerType] && theType.asInstanceOf[IPointerType].getType.isInstanceOf[IPointerType]
  }
  
  def isOnLeftSideOfAssignment(node: IASTNode) = {
    val assignmentParent = Utils.getAncestors(node).collect{ case x: IASTBinaryExpression => x}.filter(x => isAssignment(x.getOperator)).headOption
    assignmentParent.map { parent =>
      Utils.getAncestors(node).contains(parent.getOperand1) || parent.getOperand1 == node
    }.getOrElse(false)
  }

  def getUnaryTarget(node: IASTNode): Option[IASTName] = {
    if (node.isInstanceOf[IASTIdExpression]) {
      Some(node.asInstanceOf[IASTIdExpression].getName)
    } else {
      val childUnaries = node.getChildren.collect{ case x: IASTUnaryExpression => x}
      childUnaries.map(_.getOperand).map(getUnaryTarget).flatten.headOption
    }
  }
  
  def getDescendants(node: IASTNode): Seq[IASTNode] = {
    Seq(node) ++ node.getChildren.flatMap{x => getDescendants(x)}
  }

  def getTranslationUnit(code: String): IASTTranslationUnit = {
    val preprocess = Gcc.preprocess(code)

    val fileContent = FileContent.create("test", preprocess.toCharArray)
    val symbolMap = new HashMap[String, String];

    val systemIncludes = List(new File(raw"C:\Scala\Git\astViewer\app"), new File(raw"C:\MinGW\include"), new File(raw"C:\MinGW\include\GL"), new File(raw"C:\MinGW\lib\gcc\mingw32\4.6.2\include"))

    val info = new ScannerInfo(symbolMap, systemIncludes.toArray.map(_.getAbsolutePath))
    val log = new DefaultLogService()
    val opts = 8
    val includes = IncludeFileContentProvider.getEmptyFilesProvider

    val tUnit = GCCLanguage.getDefault().getASTTranslationUnit(fileContent, info, includes, null, opts, log)

    tUnit
  }
}
