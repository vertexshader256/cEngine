package scala.c.engine

import org.anarres.cpp.{InputLexerSource, Preprocessor, Token}
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression.*
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.parser.{DefaultLogService, FileContent, IncludeFileContentProvider, ScannerInfo}
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType

import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets
import java.util
import java.util.HashMap
import scala.collection.mutable.ListBuffer

object Utils {

	val rootDir = raw"C:\msys64\\ucrt64"

	val mainPath = raw"."
	val minGWIncludes = s"$rootDir\\include"

	//val includeDir = new File(s"$rootDir\\lib\\gcc\\x86_64-w64-mingw32\\15.2.0").listFiles().head.getAbsolutePath

	val minGWAdditionalIncludes = new File(s"$rootDir\\lib\\gcc\\x86_64-w64-mingw32\\15.2.0\\include").getAbsolutePath

	val minGWMoreIncludes = s"$rootDir\\include\\GL"

	def getAncestors(node: IASTNode): Seq[IASTNode] = {
		var current = node.getParent
		val parents = new ListBuffer[IASTNode]()
		while (current != null) {
			parents += current
			current = current.getParent
		}

		parents.result
	}

	private def readChar(address: Int)(implicit state: State): Char = {
		state.Stack.readFromMemory(address, CBasicType(IBasicType.Kind.eChar, 0)).value.asInstanceOf[Byte].toChar
	}

	def readString(address: Int)(implicit state: State): String = {
		var current: Char = 0
		val stringBuilder = new ListBuffer[Char]()
		var offset = 0

		current = readChar(address + offset)

		while current != 0 do
			if current != 0 then
				stringBuilder += current
				offset += 1

			current = readChar(address + offset)

		String(stringBuilder.map(_.toByte).toArray, "UTF-8")
	}

	def getTranslationUnit(code: String, includePaths: List[String]): IASTTranslationUnit = {

		val preprocessed = preprocessSource(code, includePaths)

		val symbolMap = new util.HashMap[String, String];
		val systemIncludes = Array[String]()

		val info = new ScannerInfo(symbolMap, systemIncludes)
		val log = new DefaultLogService()
		val opts = 8
		val includes = IncludeFileContentProvider.getEmptyFilesProvider

		val fileContent = FileContent.create("test", preprocessed.toCharArray)

		GCCLanguage.getDefault.getASTTranslationUnit(fileContent, info, includes, null, opts, log)
	}

	private def preprocessSource(code: String, includePaths: List[String]): String = {
		val preprocessResults = new StringBuilder

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

		val totalCode = lines.reduce(_ + "\n" + _)

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
				case e: Throwable => skipline = true; startLine = currentLine + 1
			}
		}

		preprocessResults.toString
	}
}
