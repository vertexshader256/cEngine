package scala.c.engine

import java.math.BigInteger
import scala.util.Try

object Literal {

	def parse(literal: String): ValueType = {
		if isQuoted(literal) then
			val post = encodeSpecialChars(literal)
			StringLiteral(post)
		else if isChar(literal) then
			val post = encodeSpecialChars(literal)
			RValue(post(1).toByte, TypeHelper.charType)
		else
			castNumericLiteral(literal)
	}

	private def encodeSpecialChars(str: String): String = {
		val result = new StringBuilder()

		var index = 0
		while (index < str.length - 1) {
			(str(index), Try(str(index + 1)).getOrElse(null)) match
				case ('\\', '\\') => result += '\\'; index += 2
				case ('\\', 'n') => result += '\n'; index += 2
				case ('\\', 'r') => result += '\r'; index += 2
				case ('\\', '0') => result += '\u0000'; index += 2
				case x => result += x._1; index += 1
		}

		result += str.last

		result.mkString
	}

	private def hasSuffix(string: String, suffix: String): Boolean =
		string.toLowerCase.endsWith(suffix)

	private def stripFixedPointSuffix(string: String): String = {
		val isUnsigned = hasSuffix(string, "u")
		val isLong = hasSuffix(string, "l")
		val isLongLong = hasSuffix(string, "ll")
		val isUnsignedLong = hasSuffix(string, "ul") || hasSuffix(string, "lu")
		val isUnsignedLongLong = hasSuffix(string, "ull") || hasSuffix(string, "llu")

		val charsToStrip = if isUnsignedLongLong then
			3
		else if isLongLong || isUnsignedLong then
			2
		else if isLong || isUnsigned then
			1
		else
			0

		val pre = string.take(string.length - charsToStrip).mkString

		if pre.startsWith("0x") then
			val bigInt = BigInteger(pre.drop(2), 16);
			bigInt.toString
		else
			pre
	}

	private def isQuoted(s: String) = s.head == '\"' && s.last == '\"'
	private def isChar(s: String) = s.head == '\'' && s.last == '\''
	private def isIntNumber(s: String) = s.toIntOption.isDefined
	private def isLongNumber(s: String) = s.toLongOption.isDefined

	private def castNumericLiteral(literal: String) = {
		val isFloat = hasSuffix(literal, "f")
		val isLong = hasSuffix(literal, "l")
		val isUnsignedLong = hasSuffix(literal, "ul") || hasSuffix(literal, "lu")
		val isUnsignedLongLong = hasSuffix(literal, "ull") || hasSuffix(literal, "llu")

		val lit = stripFixedPointSuffix(literal)

		if isUnsignedLongLong then
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
	}
}