package app.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ ListBuffer, Stack }
import scala.util.control.Exception.allCatch
import java.math.BigInteger
import scala.collection.mutable.Map

case class Literal(litStr: String) {
  def cast: AnyVal = {

    def isIntNumber(s: String): Boolean = (allCatch opt s.toInt).isDefined
    def isLongNumber(s: String): Boolean = (allCatch opt s.toLong).isDefined
    
    val isLong = litStr.endsWith("L")
    val isUnsignedLong = litStr.endsWith("UL")
    
    val pre: String = if (litStr.endsWith("L")) {
      litStr.take(litStr.size - 1).mkString
    } else {
      litStr
    }
    
    val lit = if (pre.startsWith("0x")) {
      val bigInt = new BigInteger(pre.drop(2), 16);
      bigInt.toString
    } else {
      pre
    }

    if (lit.head == '\"' && lit.last == '\"') {
      StringLiteral(lit)
    } else if (lit.head == '\'' && lit.last == '\'' && (lit.size == 3 || lit == "'\\0'")) {
      if (lit == "'\\0'") {
        0
      } else {
        lit.toCharArray.apply(1)
      }
    } else if (isIntNumber(lit)) {
      lit.toInt
    } else if (isLongNumber(lit)) {
      lit.toLong
    } else if (lit.contains('F') || lit.contains('f')) {
      val num = lit.toCharArray.filter(x => x != 'f' && x != 'F').mkString
      num.toFloat
    } else {
      lit.toDouble
    }
  }
  
  def typeCast(theType: IBasicType): AnyVal = {
    TypeHelper.cast(theType, cast)
  }
}