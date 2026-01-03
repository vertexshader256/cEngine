package scala.c.engine

import scala.c.engine.NumBits.ThirtyTwoBits

class HelloWorld extends StandardTest {
	"hello world" should "print the correct results" in {
		import Interpreter._
		implicit val state = new State(List(), ThirtyTwoBits)
		c"""printf("Hello World!\n");"""
		assert(getResults(state.stdout.toList) == List("Hello World!"))
	}
}