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
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType

sealed trait Direction
object Entering extends Direction
object Exiting extends Direction
object Visiting extends Direction

case class Path(node: IASTNode, direction: Direction, index: Int)

object Utils {
  
  def stripQuotes(str: String): String = {
    str.tail.reverse.tail.reverse
  }
  
  def allocateString(arg: Any, isHeap: Boolean)(implicit state: State): ValueInfo = {
    arg match {
        case StringLiteral(str) => 
          state.createStringVariable(str, isHeap)
        case x => TypeHelper.resolve(x)
      }
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
  
  def readString(address: Address)(implicit state: State): String = {
     var current: Char = 0
      var stringBuilder = new ListBuffer[Char]()
      var i = 0
      do {
        current = state.readVal(address + i, new CBasicType(IBasicType.Kind.eChar, 0)).value.asInstanceOf[Byte].toChar
        if (current != 0) {
          stringBuilder += current
          i += 1
        }
      } while (current != 0)
        
      new String(stringBuilder.map(_.toByte).toArray, "UTF-8")
  }
  
  def isAssignment(op: Int) = {
      op == op_assign ||
      op == op_plusAssign ||
      op == op_minusAssign ||
      op == op_multiplyAssign ||
      op == op_divideAssign ||
      op == op_moduloAssign ||
      op == op_binaryXorAssign ||
      op == op_binaryAndAssign ||
      op == op_binaryOrAssign ||
      op == op_multiplyAssign ||
      op == op_shiftLeftAssign ||
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

  val mainPath = raw"C:\Scala\Git\AstViewer"
  val mainAdditionalPath = raw"C:\Scala\Git\AstViewer\test\scala\libds-master"
  val minGWIncludes = raw"C:\MinGW\include"
  val minGWAdditionalIncludes = raw"C:\MinGW\lib\gcc\mingw32\4.9.3\include"
  val minGWMoreIncludes = raw"C:\MinGW\include\GL"
  
  def getTranslationUnit(codes: Seq[String]): IASTTranslationUnit = {

		val preprocessResults = new StringBuilder
		
		val newCodes = List(better.files.File("app\\ee_printf.c").contentAsString) ++ codes
		
		newCodes.map{theCode =>
		  
		  var lines = if (theCode != newCodes.head) {
		    //List("#define printf ee_printf \n") ++ theCode.split("\\r?\\n").toList
		    theCode.split("\\r?\\n").toList
		  } else {
		    List("#define HAS_FLOAT\n") ++ theCode.split("\\r?\\n").toList
		  }
		  
		  // solution to deal with var args
		  val linesWithInclude = lines.zipWithIndex.filter{case (line, index) => line.contains("#include")}
		  val lastInclude = linesWithInclude.reverse.headOption.map{case (line, index) => index + 1}.getOrElse(-1)
		  if (lastInclude != -1) {
		    lines = lines.take(lastInclude) ++ 
		       List("#define va_arg(x,y) va_arg(x, #y)\n") ++ 
		       List("#define va_start(x,y) va_start(&x, &y)\n") ++ 
		       List("#define va_end(x) va_end(x)\n") ++ 
		       lines.drop(lastInclude)
		  }
		  
		  val code = lines.reduce{_ + "\n" + _}

		  val	pp = new Preprocessor();

  		pp.getSystemIncludePath.add(minGWIncludes)
  		pp.getSystemIncludePath.add(minGWAdditionalIncludes)
  		pp.addMacro("__cdecl", "")
  		pp.getQuoteIncludePath.add(minGWIncludes)
  		pp.getQuoteIncludePath.add(mainAdditionalPath)
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

    val systemIncludes = List(new File(mainPath), new File(minGWIncludes), new File(minGWMoreIncludes), new File(minGWAdditionalIncludes))

    val info = new ScannerInfo(symbolMap, systemIncludes.toArray.map(_.getAbsolutePath))
    val log = new DefaultLogService()
    val opts = 8
    val includes = IncludeFileContentProvider.getEmptyFilesProvider

    val tUnit = GCCLanguage.getDefault().getASTTranslationUnit(fileContent, info, includes, null, opts, log)

    tUnit
  }
}
