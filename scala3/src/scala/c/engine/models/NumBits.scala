package scala.c.engine.models

enum NumBits(val ptrSize: Int) {
	case ThirtyTwoBits extends NumBits(4)
	case SixtyFourBits extends NumBits(8)
}