package scala.astViewer

import java.io.File
import java.util.HashMap

import org.eclipse.cdt.core.dom.ast.{IASTNode, _}
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.parser.{DefaultLogService, FileContent, IncludeFileContentProvider, ScannerInfo}
import scala.collection.mutable.ListBuffer

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

  def getPath(tUnit: IASTTranslationUnit): Seq[IASTNode] = {

    val results = new ListBuffer[IASTNode]()

    def recurse(node: IASTNode): Seq[IASTNode] = {
        node.getChildren.flatMap { child =>
          val x = recurse(child)
          if (!x.isEmpty) {
            Seq(node) ++ recurse(child) ++ Seq(node)
          } else {
            Seq(node)
          }
        }

    }

    val crudeList = recurse(tUnit)
    println(crudeList.size)

    var lastItem: IASTNode = null

    crudeList.foreach{item =>
      if (lastItem != item) {
        results += item
      }
      lastItem = item
    }

    results
  }
}
