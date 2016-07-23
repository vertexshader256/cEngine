import play.api.ApplicationLoader.Context
import play.api._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Results._
import play.api.mvc._
import play.api.routing.Router
import play.api.routing.sird._
import org.eclipse.cdt.internal.core.dom.parser.c._
import org.eclipse.cdt.core.dom.ast.IASTNode
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.parser._
import java.io.File
import java.util.Map
import java.util.HashMap

import scala.util.{Failure, Success, Try}
import play.api.libs.json.{JsNull, JsString, JsValue, Json}

import scala.concurrent.Future

class AppLoader extends ApplicationLoader {

  var count = 0

  def getAllChildren(node: IASTNode): JsObject = {

    def getLabel(node: IASTNode) = node match {
      case x: CASTParameterDeclaration => "Param"
      case x: CASTTypedefNameSpecifier => x.getName.getRawSignature
      case x: CASTFunctionDeclarator => "Function"
      case x: CASTFieldReference => "."
      case x: CASTFieldReference => "."
      case x: CASTFunctionDefinition => x.getDeclarator.getName.toString + "()"
      case x: CASTSimpleDeclaration => "Declaration"
      case x: CASTArrayDeclarator => "Array"
      case x: CASTDeclarator => x.getName.getRawSignature
      case x: CASTReturnStatement => "Return"
      case x: CASTIfStatement => "If"
      case x: CASTWhileStatement => "While"
      case x: CASTExpressionStatement => "Expression"
      case x: CASTUnaryExpression => "="
      case x if x.getChildren.isEmpty => node.getRawSignature
      case x => x.getClass.getSimpleName
    }

    def recurse(node: IASTNode): Seq[JsObject] = {
      count += 1
      if (count > 100) {
        Nil
      } else {
        node.getChildren.map { child =>
          if (!child.getChildren.isEmpty) {
            Json.obj(
              "name" -> getLabel(child),
              "children" -> recurse(child)
            )
          } else {
            Json.obj(
              "name" -> getLabel(child),
              "size" -> 1000
            )
          }
        }
      }
    }

    if (!node.getChildren.isEmpty) {
      Json.obj(
        "name" -> getLabel(node),
        "children" -> recurse(node)
      )
    } else {
      Json.obj(
        "name" -> getLabel(node),
        "size" -> 1000
      )
    }
  }

  def getTranslationUnit(code: String) = {
    val fileContent = FileContent.create("test", code.toCharArray)
    val symbolMap = new HashMap[String, String];

    val systemIncludes = List(new File(raw"C:\MinGW\include"), new File(raw"C:\MinGW\include\GL"), new File(raw"C:\MinGW\lib\gcc\mingw32\4.6.2\include"))

    val info = new ScannerInfo(symbolMap, systemIncludes.toArray.map(_.getAbsolutePath))
    val log = new DefaultLogService()
    val opts = 8
    val includes = IncludeFileContentProvider.getEmptyFilesProvider

    GCCLanguage.getDefault().getASTTranslationUnit(fileContent, info, includes, null, opts, log)
  }

  // Entry point!
  def load(context: Context) = new BuiltInComponentsFromContext(context) {

    val router = Router.from {

      // scripts
      case GET(p"/js/webgldev/$file*") => Action.async {
        println("-------------------------------------------------")
        Future {
            Ok.sendFile(new java.io.File(s"./app/views/Application/$file"))
        }
      }

      case GET(p"/js/$file*") => Action.async {
        Future {
          Ok.sendFile(new java.io.File(s"./public/js/$file"))
        }
      }
      
      // stylesheets
      case GET(p"/css/$file*") => Action.async {
        Future {
          Ok.sendFile(new java.io.File(s"./public/css/$file"))
        }
      }

      case GET(p"/getAst" ? q"code=$code" ? q"height=${int(height)}" ? q"width=${int(width)}") => Action.async {

        Future {
          println(code)
          val tUnit = getTranslationUnit(code)
          Ok(getAllChildren(tUnit))
        }
      }

      // images
      case GET(p"/img/$file*") => Action.async {
        Future {
          Ok.sendFile(new java.io.File(s"./public/img/$file"))
        }
      }

      case GET(p"/$page") => Action.async {
        Future {
          if (page == "favicon.ico") {
            Ok.sendFile(new java.io.File(s"./public/img/favicon.png"))
          } else {
            val f = new java.io.File(s"./public/main.html")
            Ok(scala.io.Source.fromFile(f.getCanonicalPath()).mkString).as("text/html");
          }
        }
      }

      case _ => Action.async {
        Future {
          println("ROUTER NOT FOUND!")
          Ok("FAIL")
        }
      }
      
    }
  }.application

}