package scala.c.engine

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ ListBuffer, Stack }
import scala.util.control.Exception.allCatch
import java.math.BigInteger
import scala.collection.mutable.Map
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType
import org.eclipse.cdt.internal.core.dom.parser.c.CPointerType

object Literal {
  def cast(litStr: String): ValueType = {

    def isIntNumber(s: String): Boolean = (allCatch opt s.toInt).isDefined
    def isLongNumber(s: String): Boolean = (allCatch opt s.toLong).isDefined
    
    val isLong = litStr.endsWith("L")

    val pre: String = if (litStr.endsWith("L")) {
      litStr.take(litStr.size - 1).mkString
    } else if (litStr.endsWith("u")) {
      litStr.take(litStr.size - 1).mkString
    } else {
      litStr
    }

    def process(str: String): String = {
      val x = str.split("\\\\\\\\")
      if (x.size > 1) {
        x.reduce{(x, y) => process(x) + "\\" + process(y)}
      } else {
        val x = str.split("\\\\r\\\\n")
        if (x.size > 1) {
          x.reduce{(x, y) => process(x) + '\r' + '\n' + process(y)}
        } else {
          val x = str.split("\\\\n")
          if (x.size > 1) {
            x.reduce{(x, y) => process(x) + '\n' + process(y)}
          } else {
            val x = str.split("\\\\0")
            if (x.size > 1) {
              x.reduce{(x, y) => process(x) + '\0' + process(y)}
            } else {
              x.head
            }
          }
        }
      }
    }

    val post = process(pre)

    val lit = if (post.startsWith("0x")) {
      val bigInt = new BigInteger(pre.drop(2), 16);
      bigInt.toString
    } else {
      post
    }

    val result = if (lit.head == '\"' && lit.last == '\"') {
      StringLiteral(lit)
    } else if (lit.head == '\'' && lit.last == '\'') {
      RValue(lit.toCharArray.apply(1).toByte, new CBasicType(IBasicType.Kind.eChar, 0))
    } else if (isLong) {
      RValue(lit.toLong, new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG))
    } else if (isIntNumber(lit)) {
      RValue(lit.toInt, new CBasicType(IBasicType.Kind.eInt, 0))
    } else if (isLongNumber(lit)) {
      RValue(lit.toLong, new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG))
    } else if (lit.contains('F') || lit.contains('f')) {
      val num = lit.toCharArray.filter(x => x != 'f' && x != 'F').mkString
      RValue(num.toFloat, new CBasicType(IBasicType.Kind.eFloat, 0))
    } else {
      RValue(lit.toDouble, new CBasicType(IBasicType.Kind.eDouble, 0))
    }

    result
  }
}