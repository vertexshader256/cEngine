package scala.c.engine

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.ListBuffer
import scala.util.control.Exception.allCatch
import java.math.BigInteger

import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType

import scala.util.Try

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

      val theStr = str.toCharArray.toList

      val result = new ListBuffer[Char]()

      try {

        var index = 0
        while (index < theStr.size - 1) {
          (theStr(index), Try(theStr(index + 1)).getOrElse(null)) match {
            case ('\\', '\\') => result += '\\'; index += 2
            case ('\\', 'n') => result += '\n'; index += 2
            case ('\\', 'r') => result += '\r'; index += 2
            case ('\\', '0') => result += '\0'; index += 2
            case x => result += x._1; index += 1
          }
        }
      } catch {
        case x => println("ERROR")
      }

      result += theStr.last

      result.toList.mkString
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
      new RValue(lit.toCharArray.apply(1).toByte, new CBasicType(IBasicType.Kind.eChar, 0)) {}
    } else if (isLong) {
      new RValue(lit.toLong, new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG)) {}
    } else if (isIntNumber(lit)) {
      new RValue(lit.toInt, new CBasicType(IBasicType.Kind.eInt, 0)) {}
    } else if (isLongNumber(lit)) {
      new RValue(lit.toLong, new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG)) {}
    } else if (lit.contains('F') || lit.contains('f')) {
      val num = lit.toCharArray.filter(x => x != 'f' && x != 'F').mkString
      new RValue(num.toFloat, new CBasicType(IBasicType.Kind.eFloat, 0)) {}
    } else {
      new RValue(lit.toDouble, new CBasicType(IBasicType.Kind.eDouble, 0)) {}
    }

    result
  }
}