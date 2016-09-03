package app.astViewer

import org.eclipse.cdt.core.dom.ast._
import java.util.Formatter
import java.util.Locale;

object Functions {
  def printf(context: IASTContext, args: Seq[Object]) = {
    val formatString = args.head.asInstanceOf[String].replaceAll("^\"|\"$", "")
 
    val buffer = new StringBuffer();
    val formatter = new Formatter(buffer, Locale.US);
    
    val resolvedStrings = args.tail.map{ _ match {
      case str: String => str.split("""\"""").mkString
      case x => x 
    }}.toArray
    
    formatter.format(formatString, resolvedStrings: _*)
    
    context.stdout ++= buffer.toString.split("""\\n""")
  }
}