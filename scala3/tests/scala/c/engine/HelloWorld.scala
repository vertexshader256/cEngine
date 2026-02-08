package scala.c.engine

import scala.c.engine.models.NumBits.SixtyFourBits

class HelloWorld extends StandardTest {
	"hello world" should "print the correct results" in {
		import Interpreter._
		implicit val state = new CEngine(List(), SixtyFourBits)
		c"""printf("Hello World!\n");"""
		assert(Results.getResults(state.stdout.toList) == List("Hello World!"))
	}
}