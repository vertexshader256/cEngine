package scala.c.engine

import java.math.BigInteger
import scala.util.Try

// raw string - may contains quotes, prefixes and postfixes
case class Lit(s: String) {
	def isQuoted = s.head == '\"' && s.last == '\"'
	def isChar = s.head == '\'' && s.last == '\''
	def isIntNumber = s.toIntOption.isDefined
	def isLongNumber = s.toLongOption.isDefined

	private def hasSuffix(string: String, suffix: String): Boolean =
		string.toLowerCase.endsWith(suffix)

	val isFloat = hasSuffix(s, "f")
	val isLong = hasSuffix(s, "l")
	val isUnsignedLong = hasSuffix(s, "ul") || hasSuffix(s, "lu")
	val isUnsignedLongLong = hasSuffix(s, "ull") || hasSuffix(s, "llu")
}

object Literal {

	def parse(literal: String): ValueType = {
		val lit = Lit(literal)

		if lit.isQuoted then
			val post = encodeSpecialChars(literal)
			StringLiteral(post)
		else if lit.isChar then
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



	private def castNumericLiteral(str: String) = {
		val literal = Lit(str)

		val lit = stripFixedPointSuffix(literal.s)

		if literal.isUnsignedLongLong then
			val bigInt = BigInteger(lit)
			TypeHelper.getLongLong(bigInt)
		else if literal.isUnsignedLong || literal.isLong then
			TypeHelper.getLong(lit)
		else if Lit(lit).isIntNumber then
			RValue(lit.toInt, TypeHelper.intType)
		else if Lit(lit).isLongNumber then
			TypeHelper.getLong(lit)
		else if literal.isFloat then
			val num = lit.toCharArray.filter(x => x != 'f' && x != 'F').mkString
			RValue(num.toFloat, TypeHelper.floatType)
		else
			RValue(lit.toDouble, TypeHelper.doubleType)
	}
}