package scala.c.engine.cFunctions

import scala.c.engine.{State, cEngVal}
import scala.c.engine.models.{Address, EmulatedFunction, RValue}

trait Convertable[A]:
	def convert(value: cEngVal): A

given Convertable[Byte] with
	def convert(value: cEngVal): Byte =
		value match
			case byte: Byte => byte
			case int: Int => int.toByte

given Convertable[Char] with
	def convert(value: cEngVal): Char =
		value match
			case char: Char => char
			case int: Int => int.toChar
			case byte: Byte => byte.toChar

given Convertable[Int] with
	def convert(value: cEngVal): Int =
		value match
			case int: Int => int
			case long: Long => long.toInt
			case byte: Byte => byte.toInt

given Convertable[Float] with
	def convert(value: cEngVal): Float =
		value match
			case float: Float => float
			case double: Double => double.toFloat
			case int: Int => int.toFloat

given Convertable[Double] with
	def convert(value: cEngVal): Double =
		value match
			case float: Float => float.toDouble
			case double: Double => double
			case int: Int => int.toDouble

given Convertable[Address] with
	def convert(value: cEngVal): Address =
		value match
			case int: Int => Address(int)

trait FunctionDef {
	def generate(implicit theState: State): EmulatedFunction
}

extension [T1: Convertable](baseFunc: scala.Function1[T1, Option[cEngVal]])
	def generate(name: String)(using State): EmulatedFunction =
		new OneParameterFunction[T1](name) {
			def func(param1: T1) = {
				baseFunc(param1)
			}
		}.generate

abstract class ZeroParameterFunction(name: String) extends FunctionDef {
	implicit var state: State = _
	def func(): Option[cEngVal]
	def generate(implicit theState: State): EmulatedFunction = {
		state = theState
		new EmulatedFunction(name) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				func().map(RValue(_))
			}
		}
	}
}

abstract class OneParameterFunction[P1](name: String)(using p1Convert: Convertable[P1]) extends FunctionDef {
	implicit var state: State = _
	def func(param1: P1): Option[cEngVal]
	def generate(implicit theState: State): EmulatedFunction = {
		state = theState
		new EmulatedFunction(name) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val param1 = p1Convert.convert(formattedOutputParams(0).value)
				func(param1).map(RValue(_))
			}
		}
	}
}

abstract class TwoParameterFunction[P1, P2](name: String)(using p1Convert: Convertable[P1], p2Convert: Convertable[P2]) extends FunctionDef {
	implicit var state: State = _
	def func(param1: P1, param2: P2): Option[cEngVal]
	def generate(implicit theState: State): EmulatedFunction = {
		state = theState
		new EmulatedFunction(name) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val param2 = p2Convert.convert(formattedOutputParams(0).value)
				val param1 = p1Convert.convert(formattedOutputParams(1).value)
				func(param1, param2).map(RValue(_))
			}
		}
	}
}

abstract class ThreeParameterFunction[P1, P2, P3](name: String)(using p1Convert: Convertable[P1], p2Convert: Convertable[P2], p3Convert: Convertable[P3]) extends FunctionDef {
	implicit var state: State = _
	def func(param1: P1, param2: P2, param3: P3): Option[cEngVal]
	def generate(implicit theState: State): EmulatedFunction = {
		state = theState
		new EmulatedFunction(name) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val param3 = p3Convert.convert(formattedOutputParams(0).value)
				val param2 = p2Convert.convert(formattedOutputParams(1).value)
				val param1 = p1Convert.convert(formattedOutputParams(2).value)
				func(param1, param2, param3).map(RValue(_))
			}
		}
	}
}

abstract class FourParameterFunction[P1, P2, P3, P4](name: String)(using p1Convert: Convertable[P1], p2Convert: Convertable[P2], p3Convert: Convertable[P3], p4Convert: Convertable[P4]) extends FunctionDef {
	implicit var state: State = _
	def func(param1: P1, param2: P2, param3: P3, param4: P4): Option[cEngVal]
	def generate(implicit theState: State): EmulatedFunction = {
		state = theState
		new EmulatedFunction(name) {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val param4 = p4Convert.convert(formattedOutputParams(0).value)
				val param3 = p3Convert.convert(formattedOutputParams(1).value)
				val param2 = p2Convert.convert(formattedOutputParams(2).value)
				val param1 = p1Convert.convert(formattedOutputParams(3).value)
				func(param1, param2, param3, param4).map(RValue(_))
			}
		}
	}
}