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
			(theStr(index), Try(theStr(index + 1)).getOrElse(null)) match
				case ('\\', '\\') => result += '\\'; index += 2
				case ('\\', 'n') => result += '\n'; index += 2
				case ('\\', 'r') => result += '\r'; index += 2
				case ('\\', '0') => result += '\u0000'; index += 2
				case x => result += x._1; index += 1
		}

		result += theStr.last

		result.toList.mkString
	}

	private def hasSuffix(string: String, suffix: String): Boolean =
		string.toLowerCase.endsWith(suffix)

	private def stripFixedPointSuffix(string: String): String = {
		val isUnsigned = hasSuffix(string, "u")
		val isLong = hasSuffix(string, "l")
		val isLongLong = hasSuffix(string, "ll")
		val isUnsignedLong = hasSuffix(string, "ul")
		val isUnsignedLongLong = hasSuffix(string, "ull")

		val charsToStrip = if isUnsignedLongLong then
			3
		else if isLongLong || isUnsignedLong then
			2
		else if isLong || isUnsigned then
			1
		else
			0

		val pre = string.take(string.length - charsToStrip).mkString

		val post = process(pre)

		if post.startsWith("0x") then
			val bigInt = BigInteger(pre.drop(2), 16);
			bigInt.toString
		else
			post
	}

	def cast(litStr: String): ValueType = {

		def isIntNumber(s: String) = s.toIntOption.isDefined
		def isLongNumber(s: String) = s.toLongOption.isDefined
		def isQuoted(s: String) = s.head == '\"' && s.last == '\"'
		def isChar(s: String) = s.head == '\'' && s.last == '\''

		val isFloat = hasSuffix(litStr, "f")
		val isLong = hasSuffix(litStr, "l")
		val isUnsignedLong = hasSuffix(litStr, "ul")
		val isUnsignedLongLong = hasSuffix(litStr, "ull")

		val lit = stripFixedPointSuffix(litStr)

		val result = if isQuoted(lit) then
			StringLiteral(lit)
		else if isChar(lit) then
			RValue(lit(1).toByte, TypeHelper.charType)
		else if isUnsignedLongLong then
			val bigInt = BigInteger(lit)
			TypeHelper.getLongLong(bigInt)
		else if isUnsignedLong || isLong then
			TypeHelper.getLong(lit)
		else if isIntNumber(lit) then
			RValue(lit.toInt, TypeHelper.intType)
		else if isLongNumber(lit) then
			TypeHelper.getLong(lit)
		else if isFloat then
			val num = lit.toCharArray.filter(x => x != 'f' && x != 'F').mkString
			RValue(num.toFloat, TypeHelper.floatType)
		else
			RValue(lit.toDouble, TypeHelper.doubleType)

		result
	}
}