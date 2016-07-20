import play.api.ApplicationLoader.Context
import play.api._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Results._
import play.api.mvc._
import play.api.routing.Router
import play.api.routing.sird._
import play.api.libs.iteratee._
import org.apache.commons.lang3.StringEscapeUtils
import play.api.libs.streams.Streams
import play.api.http.HttpEntity
import akka.stream.scaladsl._
import akka.util.ByteString
import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ListBuffer
import play.api.libs.json.{JsNull, Json, JsString, JsValue}
import scala.concurrent.Future


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
      
    }
  }.application

}