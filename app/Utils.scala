package app.astViewer

import java.io.File
import java.util.HashMap

import org.eclipse.cdt.core.dom.ast.{IASTNode, _}
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.parser.{DefaultLogService, FileContent, IncludeFileContentProvider, ScannerInfo}
import scala.collection.mutable.ListBuffer
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._
import org.anarres.cpp.Preprocessor
import java.nio.charset.StandardCharsets
import org.anarres.cpp.InputLexerSource
import java.io.ByteArrayInputStream
import org.anarres.cpp.Token
import better.files._

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

  def getTranslationUnit(codes: Seq[String]): IASTTranslationUnit = {

		val preprocessResults = new StringBuilder
		
		codes.map{code =>
		  
		  val	pp = new Preprocessor();

  		pp.getSystemIncludePath.add("C:\\MinGW\\include")
  		pp.getSystemIncludePath.add("C:\\MinGW\\lib\\gcc\\mingw32\\4.9.3\\include")
  		pp.addMacro("__cdecl", "")
  		pp.getQuoteIncludePath.add("C:\\MinGW\\include")
  		pp.getQuoteIncludePath.add("C:\\Scala\\Git\\AstViewer\\test\\scala\\c-algorithms-master\\src")
  		pp.getQuoteIncludePath.add("C:\\Scala\\Git\\AstViewer\\test\\scala\\c-algorithms-master\\test")
  		//pp.addMacro("ALLOC_TESTING");
		
  		val stream = new ByteArrayInputStream(code.getBytes(StandardCharsets.UTF_8))
  		
  		pp.addInput(new InputLexerSource(stream))
  		
  		var shouldBreak = false
  		var skipline = false
  		var startLine = 0
  		var currentLine = 0
  		
  		while (!shouldBreak) {
  		  try {
  				var	tok = pp.token
  				currentLine = tok.getLine
  				
  				while (skipline && currentLine == startLine) {
  				  tok = pp.token
  				  currentLine = tok.getLine
  				}
  				skipline = false
  				
  				if (tok == null)
  					shouldBreak = true
  					
  				if (!shouldBreak && tok.getType == Token.EOF)
  					shouldBreak = true
  					
  				if (!shouldBreak) {
  				  preprocessResults ++= tok.getText
  				}
  		  } catch {
  		    case e => skipline = true; startLine = currentLine + 1
  		  }
  			}
		}
		
		val preprocess = preprocessResults.toString.replaceAll("(?m)(^ *| +(?= |$))", "").replaceAll("(?m)^$([\r\n]+?)(^$[\r\n]+?^)+", "$1")

		better.files.File("what.txt").write(preprocess)

    val fileContent = FileContent.create("test", preprocess.toCharArray)
    val symbolMap = new HashMap[String, String];

    val systemIncludes = List(new File(raw"C:\Scala\AstViewer\app"), new File(raw"C:\MinGW\include"), new File(raw"C:\MinGW\include\GL"), new File(raw"C:\MinGW\lib\gcc\mingw32\4.9.3\include"))

    val info = new ScannerInfo(symbolMap, systemIncludes.toArray.map(_.getAbsolutePath))
    val log = new DefaultLogService()
    val opts = 8
    val includes = IncludeFileContentProvider.getEmptyFilesProvider

    val tUnit = GCCLanguage.getDefault().getASTTranslationUnit(fileContent, info, includes, null, opts, log)

    tUnit
  }
}
