package scala.c.engine

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType

import java.math.BigInteger
import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.control.Exception.allCatch

object Literal {
	def cast(litStr: String): ValueType = {

		def isIntNumber(s: String): Boolean = (allCatch opt s.toInt).isDefined

		def isLongNumber(s: String): Boolean = (allCatch opt s.toLong).isDefined

		val isLong = litStr.endsWith("L")

		val pre: String = if (litStr.endsWith("LL")) {
			litStr.take(litStr.size - 2).mkString
		} else if (litStr.endsWith("L")) {
			litStr.take(litStr.size - 1).mkString
		} else if (litStr.endsWith("u")) {
			litStr.take(litStr.size - 1).mkString
		} else {
			litStr
		}

		def process(str: String): String = {

			val theStr = str.toCharArray.toList

			val result = new ListBuffer[Char]()

			var index = 0
			while (index < theStr.size - 1) {
				(theStr(index), Try(theStr(index + 1)).getOrElse(null)) match {
					case ('\\', '\\') => result += '\\'; index += 2
					case ('\\', 'n') => result += '\n'; index += 2
					case ('\\', 'r') => result += '\r'; index += 2
					case ('\\', '0') => result += '\u0000'; index += 2
					case x => result += x._1; index += 1
				}
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
			RValue(lit.toCharArray.apply(1).toByte, new CBasicType(IBasicType.Kind.eChar, 0))
		} else if (isLong) {
			RValue(lit.toLong, new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG))
		} else if (isIntNumber(lit)) {
			RValue(lit.toInt, TypeHelper.intType)
		} else if (isLongNumber(lit)) {
			RValue(lit.toLong, new CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG))
		} else if (lit.contains('F') || lit.contains('f')) {
			val num = lit.toCharArray.filter(x => x != 'f' && x != 'F').mkString
			RValue(num.toFloat, TypeHelper.floatType)
		} else {
			RValue(lit.toDouble, TypeHelper.doubleType)
		}

		result
	}
}