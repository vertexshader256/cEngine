package scala.c.engine

class HelloWorld extends StandardTest {
	"hello world" should "print the correct results" in {
		import Interpreter.*
		implicit val state = new State(ThirtyTwoBits)
		c"""printf("Hello World!\n");"""
		assert(getResults(state.stdout.toList) == List("Hello World!"))
	}
}