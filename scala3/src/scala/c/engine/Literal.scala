package scala.c.engine

import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType

import java.math.BigInteger
import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.control.Exception.allCatch

object Literal {
	private def process(str: String): String = {

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

	def cast(litStr: String): ValueType = {

		def isIntNumber(s: String): Boolean = (allCatch opt s.toInt).isDefined
		def isLongNumber(s: String): Boolean = (allCatch opt s.toLong).isDefined

		val isFloat = litStr.takeRight(1).toLowerCase == "f"
		val isUnsigned = litStr.takeRight(1).toLowerCase == "u"
		val isLong = litStr.takeRight(1).toLowerCase == "l"
		val isLongLoong = litStr.takeRight(2).toLowerCase == "ll"
		val isUnsignedLong = litStr.takeRight(2).toLowerCase == "ul"
		val isUnsignedLongLong = litStr.takeRight(3).toLowerCase == "ull"

		val pre: String = if (isUnsignedLongLong) {
			litStr.take(litStr.length - 3).mkString
		} else if (isLongLoong) {
			litStr.take(litStr.length - 2).mkString
		} else if (isUnsignedLong) {
			litStr.take(litStr.length - 2).mkString
		} else if (isLong) {
			litStr.take(litStr.length - 1).mkString
		} else if (isUnsigned) {
			litStr.take(litStr.length - 1).mkString
		} else {
			litStr
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
		} else if (isUnsignedLongLong) {
			TypeHelper.getLongLong(lit)
		} else if (isUnsignedLong) {
			TypeHelper.getLong(lit)
		} else if (isLong) {
			TypeHelper.getLong(lit)
		} else if (isIntNumber(lit)) {
			RValue(lit.toInt, TypeHelper.intType)
		} else if (isLongNumber(lit)) {
			TypeHelper.getLong(lit)
		} else if (isFloat) {
			val num = lit.toCharArray.filter(x => x != 'f' && x != 'F').mkString
			RValue(num.toFloat, TypeHelper.floatType)
		} else {
			RValue(lit.toDouble, TypeHelper.doubleType)
		}

		result
	}
}