package scala.c.engine

class LowLevel extends StandardTest {
  "ensure the stack gets popped after functions" should "print the correct results" in {
    import Interpreter._

    implicit val state = new State(ThirtyTwoBits)

    val start = state.Stack.insertIndex

    c"""isalpha('5');"""

    assert(state.Stack.insertIndex == start)
  }

  "ensure the stack gets popped after functions with string args" should "print the correct results" in {
    import Interpreter._
    implicit val state = new State(ThirtyTwoBits)

    val start = state.Stack.insertIndex

    c"""printf("%d\n", 5);"""

    assert(state.Stack.insertIndex == start)
  }
}

class VariableLowLevelTest extends StandardTest {
  "ensure ints are 4 bytes" should "print the correct results" in {
    import Interpreter._

    implicit val state = new State(ThirtyTwoBits)

    val start = state.Stack.insertIndex

    c"""int i = 10;"""

    assert(state.Stack.insertIndex == start + 4)
  }

  "ensure doubles are 8 bytes" should "print the correct results" in {
    import Interpreter._
    implicit val state = new State(ThirtyTwoBits)

    val start = state.Stack.insertIndex

    c"""double i = 10.0;"""

    assert(state.Stack.insertIndex == start + 8)
  }

  "ensure chars are 1 byte" should "print the correct results" in {
    import Interpreter._
    implicit val state = new State(ThirtyTwoBits)

    val start = state.Stack.insertIndex

    c"""char i = 'a';"""

    assert(state.Stack.insertIndex == start + 1)
  }
}
