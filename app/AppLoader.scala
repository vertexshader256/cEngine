package app.astViewer

import play.api.ApplicationLoader.Context
import play.api._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Results._
import play.api.mvc._
import play.api.routing.Router
import play.api.routing.sird._
import org.eclipse.cdt.internal.core.dom.parser.c._
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.parser._
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression._


import scala.util.{Failure, Success, Try}
import play.api.libs.json.{JsNull, JsString, JsValue, Json}



import scala.concurrent.Future

object AstUtils {
  def getAllChildren(node: IASTNode): JsObject = {

    var count = 0

    def getLabel(node: IASTNode) = node match {
      case x: CASTParameterDeclaration => "Param"
      case x: CASTTypedefNameSpecifier => x.getName.getRawSignature
      case x: CASTFunctionDeclarator => "Function"
      case x: CASTIdExpression => ""
      case x: CASTFieldReference => "."
      case x: CASTFunctionCallExpression => "calls"
      case x: CASTFunctionDefinition => x.getDeclarator.getName.toString + "()"
      case x: CASTSimpleDeclaration => ""
      case x: CASTArrayDeclarator => "Array"
      case x: CASTDeclarator => ""
      case x: CASTReturnStatement => "return"
      case x: CASTIfStatement => "if"
      case x: CASTForStatement => "for"
      case x: CASTWhileStatement => "while"
      case x: CASTExpressionStatement => ""
      case x: CASTCompoundStatement => ""
      case x: CASTUnaryExpression => ""
      case x: CASTEqualsInitializer => "="
      case x: CASTDeclarationStatement => ""
      case x: CASTTranslationUnit => ""
      case x: CASTName => x.toString
      case x if x.getChildren.isEmpty => node.getRawSignature
      case expr: CASTBinaryExpression => {
        expr.getOperator match {
          case `op_assign` => "="
          case `op_binaryAnd` => "&"
          case `op_binaryAndAssign` => "&="
          case `op_binaryOr` => "|"
          case `op_binaryOrAssign` => "|="
          case `op_binaryXor` => "^"
          case `op_binaryXorAssign` => "^="
          case `op_divide` => "/"
          case `op_divideAssign` => "/="
          case `op_ellipses` => "..."
          case `op_equals` => "=="
          case `op_greaterEqual` => ">="
          case `op_greaterThan` => ">"
          case `op_lessEqual` => "<="
          case `op_lessThan` => "<"
          case `op_logicalAnd` => "&&"
          case `op_logicalOr` => "||"
          case `op_minus` => "-"
          case `op_minusAssign` => "-="
          case `op_modulo` => "%"
          case `op_moduloAssign` => "%="
          case `op_multiply` => "*"
          case `op_multiplyAssign` => "*="
          case `op_notequals` => "!="
          case `op_plus` => "+"
          case `op_plusAssign` => "+="
          case `op_shiftLeft` => "<<"
          case `op_shiftLeftAssign` => "<<="
          case `op_shiftRight` => ">>"
          case `op_shiftRightAssign` => ">>="
          case _ => "UNKNOWN BINARY EXPRESSION"
        }
      }
      case x => x.getClass.getSimpleName
    }

    def recurse(node: IASTNode): Seq[JsObject] = {
      count += 1
      if (count > 100) {
        Nil
      } else {
        node.getChildren.map { child =>
          Json.obj(
            "name" -> getLabel(child),
            "type" -> child.getClass.getSimpleName,
            "children" -> recurse(child),
            "offset" -> (if (child.getFileLocation != null) child.getFileLocation.getNodeOffset else 0),
            "length" -> child.getRawSignature.length
          )
        }
      }
    }

    Json.obj(
      "name" -> getLabel(node),
      "type" -> node.getClass.getSimpleName,
      "children" -> recurse(node),
      "offset" -> (if (node.getFileLocation != null) node.getFileLocation.getNodeOffset else 0),
      "length" -> node.getRawSignature.length
    )

    // "location" -> Json.obj("offset" -> node.getFileLocation.getOffset, length -> node.getRawSignature.length)
  }


}

class AppLoader extends ApplicationLoader {

  

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
        println("GETTING AST")
        Future {
          val tUnit = Utils.getTranslationUnit(code)
          
          Ok(AstUtils.getAllChildren(tUnit))
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