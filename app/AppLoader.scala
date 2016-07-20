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

import scala.util.{Failure, Success, Try}
import play.api.libs.json.{JsNull, JsString, JsValue, Json}
import scala.concurrent.Future

class AppLoader extends ApplicationLoader {

  trait d3Node
  case class d3Leaf(name: String, size: Double) extends d3Node
  case class d3Package(name: String, children: Seq[d3Node]) extends d3Node

  var count = 0



  def getAllChildren(node: IASTNode): d3Node = {

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

    def recurse(node: IASTNode): Seq[d3Node] = {
      count += 1
      if (count > 100) {
        Nil
      } else {
        node.getChildren.map { child =>
          if (!child.getChildren.isEmpty) {
            d3Package(getLabel(child), recurse(child))
          } else {
            d3Leaf(getLabel(child), 1000)
          }
        }
      }
    }

    if (!node.getChildren.isEmpty) {
      d3Package(getLabel(node), recurse(node))
    } else {
      d3Leaf(getLabel(node), 1000)
    }
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

        println("GETTING AST!")

        Future {

          val nodes = Json.arr(
            Json.obj(
              "x" -> width/3,
              "y" -> height/2
            ),
            Json.obj(
              "x" -> 2*width/3,
              "y" -> height/2
            )
          )

          Ok(Json.stringify(nodes))
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
            println("SERVING INDEX")
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