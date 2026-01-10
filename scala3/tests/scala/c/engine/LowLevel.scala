package scala.c.engine

import scala.c.engine.models.NumBits.ThirtyTwoBits

class LowLevel extends StandardTest {
	"ensure the stack gets popped after functions" should "print the correct results" in {
		import Interpreter._

		implicit val state = new State(List(), ThirtyTwoBits)

		val start = state.Stack.insertIndex

		c"""isalpha('5');"""

		assert(state.Stack.insertIndex == start)
	}
}

class LowLevel2 extends StandardTest {
	"ensure the stack gets popped after functions with string args" should "print the correct results" in {
		import Interpreter._
		implicit val state = new State(List(), ThirtyTwoBits)

		val start = state.Stack.insertIndex

		c"""printf("%d\n", 5);"""

		assert(state.Stack.insertIndex == start)
	}
}

class LowLevel3 extends StandardTest {
	"Making sure custom functions pop stack" should "print the correct results" in {
		import Interpreter._
		implicit val state = new State(List(), ThirtyTwoBits)

		func"""
      void add() {
          char *s = "testtest";
          printf("%s\n", s);
      }"""

		val start = state.Stack.insertIndex

		c"""
       printf("%d\n", add());
     """

		assert(state.Stack.insertIndex == start)
	}
}

class LowLevel4 extends StandardTest {
	"Making sure parameters are cleared from stack" should "print the correct results" in {
		import Interpreter._
		implicit val state = new State(List(), ThirtyTwoBits)

		func"""
      int add(char *s) {
          printf("%s\n", s);
          return 15;
      }"""

		val start = state.Stack.insertIndex

		c"""
       printf("%d\n", add("testtest"));
     """

		assert(state.Stack.insertIndex == start)
	}
}

class VariableLowLevelTest extends StandardTest {
	"ensure ints are 4 bytes" should "print the correct results" in {
		import Interpreter._

		implicit val state = new State(List(), ThirtyTwoBits)

		val start = state.Stack.insertIndex

		c"""int i = 10;"""

		assert(state.Stack.insertIndex == start + 4)
	}
}

class VariableLowLevelTest2 extends StandardTest {
	"ensure doubles are 8 bytes" should "print the correct results" in {
		import Interpreter._
		implicit val state = new State(List(), ThirtyTwoBits)

		val start = state.Stack.insertIndex

		c"""double i = 10.0;"""

		assert(state.Stack.insertIndex == start + 8)
	}
}

class VariableLowLevelTest3 extends StandardTest {
	"ensure chars are 1 byte" should "print the correct results" in {
		import Interpreter._
		implicit val state = new State(List(), ThirtyTwoBits)

		val start = state.Stack.insertIndex

		c"""char i = 'a';"""

		assert(state.Stack.insertIndex == start + 1)
	}
}
