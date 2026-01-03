package scala.c.engine

import org.anarres.cpp.{InputLexerSource, Preprocessor, Token}
import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets

object Preprocessor {
	
	private val rootDir = raw"C:\msys64\\ucrt64"
	private val minGWAdditionalIncludes = new File(s"$rootDir\\lib\\gcc\\x86_64-w64-mingw32\\15.2.0\\include").getAbsolutePath
	private val minGWIncludes = s"$rootDir\\include"

	def preprocess(code: String, includePaths: List[String]): String = {
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
