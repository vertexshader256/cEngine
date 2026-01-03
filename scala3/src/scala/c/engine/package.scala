package scala.c

package object engine {
	type char = Byte
	type int = Int
	type short = Short
	type long = Int
	type double = Double
	type float = Float
	type FixedPoint = Byte | Int | Short | Long | BigInt
	type FloatingPoint = Double | Float
	type cEngVal = Boolean | FixedPoint | FloatingPoint
}