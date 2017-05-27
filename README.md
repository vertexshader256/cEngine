# cEngine

No bytecode generation, no [JNI](https://en.wikipedia.org/wiki/Java_Native_Interface). Run C directly during runtime on the JVM.
<img align="right" src="cEngineLogo.png" width="100">

Supports:
- Multi-dimensional arrays
- Variable arguments
- Pointer aritmetic
- Recursion
- Multilevel pointers
- Function pointers
- Unsigned types
- Memory allocation
- Structures, enumerations

# sbt
```scala
libraryDependencies += "com.github.bdwashbu" % "cengine_2.11" % "0.0.5"
```

# Hello world

From the sbt console:
```scala
scala> import c.engine.Interpreter._
scala> c"""printf("Hello world!");"""
Hello world!
```

# Dependencies

* [Eclipse CDT](https://eclipse.org/cdt/)
* [JCPP](http://www.anarres.org/projects/jcpp/)

# Test approach:
For all code, standard output of cEngine must exactly match that of gcc.  Tested against gcc 5.3.0.

[scalatest](https://github.com/scalatest/scalatest) and [sbt-coverage](https://github.com/scoverage/sbt-scoverage) are very helpful.

cEngine was bootstrapped off of [java.util.Formatter](https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html) but it can now execute an embedded version of printf, ee_printf.

ee_printf is found here:
https://github.com/bwalex/supermips/blob/master/software/coremark/supermips/ee_printf.c

# Debugging

Using d3.js to visualize the AST is very helpful.  

[cEngine Debugger](https://github.com/bdwashbu/cEngine_debugger)

# Assumptions
All code executed by cEngine can be succesfully compiled with gcc.  
