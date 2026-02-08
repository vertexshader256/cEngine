package scala.c.engine

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.parser.{DefaultLogService, FileContent, IncludeFileContentProvider, ScannerInfo}
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType

import java.io.File
import java.util
import scala.c.engine.models.Address
import scala.collection.mutable.ListBuffer

object Utils {

	val rootDir = raw"C:\msys64\\ucrt64"
	val mainPath = raw"."

	val minGWIncludes = s"$rootDir\\include"
	val minGWAdditionalIncludes: String = File(s"$rootDir\\lib\\gcc\\x86_64-w64-mingw32\\15.2.0\\include").getAbsolutePath
	val minGWMoreIncludes = s"$rootDir\\include\\GL"

	def getTranslationUnits(codes: Seq[String], includePaths: List[String]): List[IASTTranslationUnit] = {
		codes.map { code => getTranslationUnit(code, includePaths) }.toList
	}

	private def getTranslationUnit(code: String, includePaths: List[String]): IASTTranslationUnit = {
		val preprocessed = Preprocessor.preprocess(code, includePaths)

		val symbolMap = util.HashMap[String, String];
		val systemIncludes = Array[String]()

		val info = ScannerInfo(symbolMap, systemIncludes)
		val log = DefaultLogService()
		val opts = 8
		val includes = IncludeFileContentProvider.getEmptyFilesProvider

		val fileContent = FileContent.create("test", preprocessed.toCharArray)

		GCCLanguage.getDefault.getASTTranslationUnit(fileContent, info, includes, null, opts, log)
	}

	def getAncestors(node: IASTNode): Seq[IASTNode] = {
		var current = node.getParent
		val parents = ListBuffer[IASTNode]()
		while (current != null) {
			parents += current
			current = current.getParent
		}

		parents.result
	}

	def getDescendants(node: IASTNode): Seq[IASTNode] = {
		node +: node.getChildren.toList.flatMap(getDescendants)
	}

	private def readChar(address: Address)(implicit state: CEngine): Char = {
		val value = state.memory.readFromMemoryRaw(TypeHelper.charType, address)
		TypeHelper.castSign(TypeHelper.charType, value).value.asInstanceOf[Byte].toChar
	}

	def readString(address: Address)(implicit state: CEngine): String = {
		var current: Char = 0
		val stringBuilder = ListBuffer[Char]()
		var offset = 0

		current = readChar(address + offset)

		while current != 0 do
			stringBuilder += current
			offset += 1
			current = readChar(address + offset)

		String(stringBuilder.map(_.toByte).toArray, "UTF-8")
	}
}
