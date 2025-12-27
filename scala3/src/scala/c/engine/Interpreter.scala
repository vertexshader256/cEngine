package scala.c.engine

object Interpreter {
	implicit class CounterSC(val sc: StringContext) extends AnyVal {

		// Define functions that we want to use with string interpolation syntax
		def c(args: Any*)(implicit state: State): Unit = {
			Gcc.runCode(sc.parts.iterator.next, state, Iterator())
		}

		def func(args: Any*)(implicit state: State): Unit = {
			Gcc.runGlobalCode(sc.parts.iterator.next, state, List())
		}
	}
}
