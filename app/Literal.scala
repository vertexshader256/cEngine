package app.astViewer

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ ListBuffer, Stack }
import scala.util.control.Exception.allCatch
import java.math.BigInteger
import scala.collection.mutable.Map
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType

case class Literal(litStr: String) {
  def cast: ValueInfo = {

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
      ValueInfo(StringLiteral(lit), new CBasicType(IBasicType.Kind.eChar, 0))
    } else if (lit.head == '\'' && lit.last == '\'' && (lit.size == 3 || lit == "'\\0'")) {
      if (lit == "'\\0'") {
        ValueInfo(0, new CBasicType(IBasicType.Kind.eFloat, 0))
      } else {
        ValueInfo(lit.toCharArray.apply(1).toByte, new CBasicType(IBasicType.Kind.eChar, 0))
      }
    } else if (isIntNumber(lit)) {
      ValueInfo(lit.toInt, new CBasicType(IBasicType.Kind.eInt, 0))
    } else if (isLongNumber(lit)) {
      ValueInfo(lit.toLong, new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG))
    } else if (lit.contains('F') || lit.contains('f')) {
      val num = lit.toCharArray.filter(x => x != 'f' && x != 'F').mkString
      ValueInfo(num.toFloat, new CBasicType(IBasicType.Kind.eFloat, 0))
    } else {
      ValueInfo(lit.toDouble, new CBasicType(IBasicType.Kind.eDouble, 0))
    }
  }
  
  def typeCast(theType: IBasicType): ValueInfo = {
    TypeHelper.cast(theType, cast.value)
  }
}