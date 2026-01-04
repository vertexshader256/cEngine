package scala.c.engine

import java.math.BigInteger
import scala.util.Try

// raw string - may contains quotes, prefixes and postfixes
case class Lit(s: String) {
	val isQuoted = s.head == '\"' && s.last == '\"'
	val isChar = s.head == '\'' && s.last == '\''
	val isIntNumber = s.toIntOption.isDefined
	val isLongNumber = s.toLongOption.isDefined

	val isUnsignedViaSuffix = hasSuffix(s, "u")
	val isLongLongViaSuffix = hasSuffix(s, "ll")

	val isFloatViaSuffix = hasSuffix(s, "f")
	val isLongViaSuffix = hasSuffix(s, "l")
	val isUnsignedLongViaSuffix = hasSuffix(s, "ul") || hasSuffix(s, "lu")
	val isUnsignedLongLongViaSuffix = hasSuffix(s, "ull") || hasSuffix(s, "llu")

	private def hasSuffix(string: String, suffix: String): Boolean =
		string.toLowerCase.endsWith(suffix)
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

	private def stripFixedPointSuffix(literal: Lit): String = {
		val charsToStrip = if literal.isUnsignedLongLongViaSuffix then
			3
		else if literal.isLongLongViaSuffix || literal.isUnsignedLongViaSuffix then
			2
		else if literal.isLongViaSuffix || literal.isUnsignedViaSuffix then
			1
		else
			0

		val withoutSuffix = literal.s.take(literal.s.length - charsToStrip).mkString

		if withoutSuffix.startsWith("0x") then
			val bigInt = BigInteger(withoutSuffix.drop(2), 16);
			bigInt.toString
		else
			withoutSuffix
	}

	private def castNumericLiteral(str: String) = {
		val literal = Lit(str)

		val lit = stripFixedPointSuffix(literal)

		if literal.isUnsignedLongLongViaSuffix then
			val bigInt = BigInteger(lit)
			TypeHelper.getLongLong(bigInt)
		else if literal.isUnsignedLongViaSuffix || literal.isLongViaSuffix then
			TypeHelper.getLong(lit)
		else if Lit(lit).isIntNumber then
			RValue(lit.toInt, TypeHelper.intType)
		else if Lit(lit).isLongNumber then
			TypeHelper.getLong(lit)
		else if literal.isFloatViaSuffix then
			val num = lit.toCharArray.filter(x => x != 'f' && x != 'F').mkString
			RValue(num.toFloat, TypeHelper.floatType)
		else
			RValue(lit.toDouble, TypeHelper.doubleType)
	}
}