package scala.c.engine

import java.math.BigInteger
import scala.util.Try

// raw string - may contains quotes, prefixes and postfixes
case class Lit(s: String) {
	val isUtf8 = s.startsWith("u8")
	val isQuoted = s.head == '\"' && s.last == '\"'
	val isChar = s.head == '\'' && s.last == '\''
	val isInt = s.toIntOption.isDefined
	val isLong = s.toLongOption.isDefined
	val isRawFloatingPoint = s.toDoubleOption.isDefined
	val isHex = s.startsWith("0x")

	val isUnsignedViaSuffix = hasSuffix(s, "u")
	val isLongLongViaSuffix = hasSuffix(s, "ll")

	val isFloatViaSuffix = hasSuffix(s, "f")
	val isLongViaSuffix = hasSuffix(s, "l")
	val isUnsignedLongViaSuffix = hasSuffix(s, "ul") || hasSuffix(s, "lu")
	val isUnsignedLongLongViaSuffix = hasSuffix(s, "ull") || hasSuffix(s, "llu")

	val isLongLong: Boolean = Try {
		if isHex then {
			BigInt(s.drop(2), 16) > BigInt(2147483647)
		} else
			BigInt(s) > BigInt(2147483647)
	}.getOrElse(false)

	val isFP = isFloatViaSuffix || isRawFloatingPoint
	val isFixedPoint = isLong || isUnsignedViaSuffix || isLongLongViaSuffix ||
		isLongViaSuffix || isUnsignedLongViaSuffix || isUnsignedLongLongViaSuffix || isHex

	private def hasSuffix(string: String, suffix: String): Boolean =
		string.toLowerCase.endsWith(suffix)

	def convertHex: String = {
		BigInteger(s.drop(2), 16).toString
	}

	def stripSuffix: String = {
		val charsToStrip = if isUnsignedLongLongViaSuffix then
			3
		else if isLongLongViaSuffix || isUnsignedLongViaSuffix then
			2
		else if isLongViaSuffix || isUnsignedViaSuffix then
			1
		else
			0

		s.take(s.length - charsToStrip).mkString
	}

	def stripFixedPointSuffix: String = {
		val withoutSuffix = Lit(stripSuffix)

		if isHex then
			withoutSuffix.convertHex
		else
			withoutSuffix.s
	}

	def stripFloatingPointSuffix: String = {
		if isFloatViaSuffix then
			s.toCharArray.filter(x => x != 'f' && x != 'F').mkString
		else
			s
	}
}

object Literal {

	def parse(literal: String): ValueType = {
		val lit = Lit(literal)

		if lit.isUtf8 then
			val post = lit.s.drop(2)
			StringLiteral(post)
		else if lit.isQuoted then
			val post = encodeSpecialChars(literal)
			StringLiteral(post)
		else if lit.isChar then
			val post = encodeSpecialChars(literal)
			RValue(post(1).toByte, TypeHelper.charType)
		else {
			castNumericLiteral(literal)
		}
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

	private def castNumericLiteral(str: String): RValue = {
		val literal = Lit(str)

		if literal.isFixedPoint then
			val lit = literal.stripFixedPointSuffix

			val value: cEngVal = if literal.isUnsignedLongLongViaSuffix || literal.isLongLong then
				BigInteger(lit)
			else if literal.isUnsignedLongViaSuffix || literal.isLongViaSuffix then
				lit.toLong
			else if Lit(lit).isInt then
				lit.toInt
			else
				lit.toLong

			TypeHelper.getRValue(value)
		else
			val lit = literal.stripFloatingPointSuffix

			val value = if literal.isFloatViaSuffix then
				lit.toFloat
			else
				lit.toDouble
				
			TypeHelper.getRValue(value)
	}
}