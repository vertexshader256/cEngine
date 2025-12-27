package scala.c.engine

import org.anarres.cpp.{InputLexerSource, Preprocessor, Token}
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.core.parser.{DefaultLogService, FileContent, IncludeFileContentProvider, ScannerInfo}
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType

import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets
import java.util.HashMap
import scala.collection.mutable.ListBuffer

sealed abstract class Direction

object Stage1 extends Direction

object Stage2 extends Direction

object Stage3 extends Direction

object PreLoop extends Direction

object Exiting extends Direction

object Gotoing extends Direction

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
		results.toSeq
	}

	def readString(address: Int)(implicit state: State): String = {
		var current: Char = 0
		var stringBuilder = new ListBuffer[Char]()
		var i = 0
		do {
			current = state.Stack.readFromMemory(address + i, new CBasicType(IBasicType.Kind.eChar, 0)).value.asInstanceOf[Byte].toChar
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

	def getDescendants(node: IASTNode): Seq[IASTNode] = {
		Seq(node) ++ node.getChildren.toList.flatMap { x => getDescendants(x) }
	}

	val rootDir = raw"C:\msys64\\ucrt64"

	val mainPath = raw"."
	val minGWIncludes = s"$rootDir\\include"

	//val includeDir = new File(s"$rootDir\\lib\\gcc\\x86_64-w64-mingw32\\15.2.0").listFiles().head.getAbsolutePath

	val minGWAdditionalIncludes = new File(s"$rootDir\\lib\\gcc\\x86_64-w64-mingw32\\15.2.0\\include").getAbsolutePath

	val minGWMoreIncludes = s"$rootDir\\include\\GL"

	def getTranslationUnit(code: String, includePaths: List[String]): IASTTranslationUnit = {

		val preprocessResults = new StringBuilder

		val totalCode = {

			var lines = code.split("\\r?\\n").toList

			// solution to deal with var args
			val linesWithInclude = lines.zipWithIndex.filter { case (line, index) => line.contains("#include") }
			val lastInclude = linesWithInclude.reverse.headOption.map { case (line, index) => index + 1 }.getOrElse(-1)
			if (lastInclude != -1) {
				lines = lines.take(lastInclude) ++
					// eclipse cdt cant handle function string args that aren't in quotes
					List("#define va_arg(x,y) va_arg(x, #y)\n") ++
					List("#define va_start(x,y) va_start(&x, &y)\n") ++
					List("#define va_end(x) va_end(x)\n") ++
					List("#define __builtin_offsetof(x, y) offsetof(#x, #y)") ++
					lines.drop(lastInclude)
			}

			lines.reduce {
				_ + "\n" + _
			}
		}

		val pp = new Preprocessor();

		pp.getSystemIncludePath.add(minGWIncludes)
		pp.getSystemIncludePath.add(minGWAdditionalIncludes)
		pp.addMacro("__cdecl", "")
		pp.addMacro("__int64", "long long") // 12-25-25: need this
		pp.addMacro("__forceinline", "") // 12-25-25: need this
		includePaths.foreach { include =>
			pp.getQuoteIncludePath.add(include)
		}

		val stream = new ByteArrayInputStream(totalCode.getBytes(StandardCharsets.UTF_8))

		pp.addInput(new InputLexerSource(stream))

		var shouldBreak = false
		var skipline = false
		var startLine = 0
		var currentLine = 0
		var justHadLineBreak = false

		while (!shouldBreak) {
			try {
				var tok = pp.token
				currentLine = tok.getLine

				while (skipline && currentLine == startLine) {
					tok = pp.token
					currentLine = tok.getLine
				}
				skipline = false

				if (tok == null || (!shouldBreak && tok.getType == Token.EOF))
					shouldBreak = true

				if (!shouldBreak) {

					if (tok.getType == Token.NL) {
						if (!justHadLineBreak) {
							justHadLineBreak = true
							preprocessResults ++= tok.getText
						}
					} else if (tok.getType == Token.WHITESPACE) {
						if (!justHadLineBreak) {
							preprocessResults ++= tok.getText
						}
					} else if (tok.getType == Token.CCOMMENT) {
						justHadLineBreak = false
					} else if (tok.getType == Token.CPPCOMMENT) {
						justHadLineBreak = false
					} else if (tok.getType == Token.IDENTIFIER) {
						if (tok.getText.startsWith("__declspec")) {
							startLine = currentLine
							while (currentLine == startLine) {
								tok = pp.token
								currentLine = tok.getLine
							}
							preprocessResults ++= tok.getText
							justHadLineBreak = false
						} else {
							preprocessResults ++= tok.getText
							justHadLineBreak = false
						}
					} else {
						preprocessResults ++= tok.getText
						justHadLineBreak = false
					}
				}
			} catch {
				case e => skipline = true; startLine = currentLine + 1
			}
		}

		val preprocess = preprocessResults.toString.replaceAll("(?m)(^ *| +(?= |$))", "").replaceAll("(?m)^$([\r\n]+?)(^$[\r\n]+?^)+", "$1")

		//    import java.io._
		//    val pw = new PrintWriter(new File("hello.txt" ))
		//    pw.write(preprocess)
		//    pw.close

		val symbolMap = new HashMap[String, String];
		val systemIncludes = Array[String]()

		val info = new ScannerInfo(symbolMap, systemIncludes)
		val log = new DefaultLogService()
		val opts = 8
		val includes = IncludeFileContentProvider.getEmptyFilesProvider

		val fileContent = FileContent.create("test", preprocess.toCharArray)

		GCCLanguage.getDefault().getASTTranslationUnit(fileContent, info, includes, null, opts, log)
	}
}
