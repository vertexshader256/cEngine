package cEngine

class HelloWorld extends StandardTest {
  "hello world" should "print the correct results" in {
    import Interpreter._
    implicit val state = new State
    c"""printf("Hello World!\n");"""
    assert(state.stdout == List("Hello World!"))
  }
}