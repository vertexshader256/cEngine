package scala.c

package object engine {
	type char = Byte
	type int = Int
	type short = Short
	type long = Int
	type double = Double
	type float = Float
	type cEngVal = AnyVal | BigInt
}