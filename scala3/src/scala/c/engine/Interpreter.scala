package scala.c.engine

import scala.c.engine.gcc.*

object Interpreter {
	implicit class CounterSC(val sc: StringContext) extends AnyVal {

		// Define functions that we want to use with string interpolation syntax
		def c(args: Any*)(implicit cEngine: CEngine): Unit = {
			cEngine.runCode(sc.parts.iterator.next, Iterator())
		}

		def func(args: Any*)(implicit cEngine: CEngine): Unit = {
			Gcc.runGlobalCode(sc.parts.iterator.next, cEngine, List())
		}
	}
}
