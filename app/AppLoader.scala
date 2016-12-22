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
      case x: CASTUnaryExpression => x.getRawSignature + "(" + x.getOperator + ")"
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
            "label" -> getLabel(child),
            "id" -> child.hashCode,
            "type" -> child.getClass.getSimpleName,
            "children" -> recurse(child),
            "offset" -> (if (child.getFileLocation != null) child.getFileLocation.getNodeOffset else 0),
            "length" -> child.getRawSignature.length
          )
        }
      }
    }

    Json.obj(
      "label" -> getLabel(node),
      "id" -> node.hashCode,
      "type" -> node.getClass.getSimpleName,
      "children" -> recurse(node),
      "offset" -> (if (node.getFileLocation != null) node.getFileLocation.getNodeOffset else 0),
      "length" -> node.getRawSignature.length
    )

    // "location" -> Json.obj("offset" -> node.getFileLocation.getOffset, length -> node.getRawSignature.length)
  }


}

 import play.api.mvc._
  import akka.stream.scaladsl._
  import akka.actor._
  import play.api.libs.streams.ActorFlow
  import akka.actor.ActorSystem
  import akka.stream.Materializer
import akka.actor._
import akka.stream._
import play.api.libs.ws.ahc._
  
 
  
class AppLoader extends ApplicationLoader {

 

  var executor: Executor = null
  var state: State = null
  
 class AkkaWebSockets(implicit system: ActorSystem, mat: Materializer) extends Controller {

    object MyWebSocketActor {
      def props(out: ActorRef) = Props(new MyWebSocketActor(out))
    }
    
    class MyWebSocketActor(out: ActorRef) extends Actor {
      def receive = {
        case msg: String =>
          if (msg == "Step") {
            executor.tick(state)
            out ! ("Step Response:" + executor.current.hashCode)
          } else if (msg.startsWith("Get Node Class Name:")) {
            val id = msg.split(":").last.trim.toInt
            
            val node = Utils.getDescendants(executor.tUnit).find{x => x.hashCode == id}.get
            
            out ! ("Current Node Class:" + node.getClass.getSimpleName)
          } else {
            out ! ("Unexpected request: " + msg)
          }
          
      }
    }
     
    def socket = WebSocket.accept[String, String] { request =>
      ActorFlow.actorRef { out =>
        MyWebSocketActor.props(out)
      }
    }
  } 
  
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
   
  val app = new AkkaWebSockets()
  
  
  
//  def socket =  WebSocket.accept[String, String] { request =>
//
//    var response: String = "Hello!"
//    
//    val in = Sink.foreach[String]{ msg =>
//      println(msg)
//      if (msg == "Step") {
//        println("ticking")
//        executor.tick()
//        println("CURRENT: " + executor.current)
//        response = "Step Response: " + executor.current.hashCode
//      } else {
//        println("NOT STEP")
//        response = "Unknown: " + msg
//      }
//    }
//
//    // Send a single 'Hello!' message and then leave the socket open
//    val out = Source.single("Step Response: 123").concat(Source.maybe)
//  
//    Flow.fromSinkAndSource(in, out)
//  }
  
  // Entry point!
  def load(context: Context) = new BuiltInComponentsFromContext(context) {

    val router = Router.from {
      
      case GET(p"/websocket") =>
        app.socket

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
          executor = new Executor()
          val state = executor.init(code, true)

          Ok(AstUtils.getAllChildren(executor.tUnit))
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