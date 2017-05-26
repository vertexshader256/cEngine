# cEngine

No bytecode generation, no [JNI](https://en.wikipedia.org/wiki/Java_Native_Interface). Run C directly during runtime on the JVM.

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

# Dependencies

* [Eclipse CDT](https://eclipse.org/cdt/)
* [JCPP](http://www.anarres.org/projects/jcpp/)

# Test approach:
Standard output of CEngine must exactly match that of GCC.  [sbt-coverage](https://github.com/scoverage/sbt-scoverage) has helped extensively.

CEngine was bootstrapped off of [java.util.Formatter](https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html) but it can now execute an embedded version of printf, ee_printf.

ee_printf is found here:
https://github.com/bwalex/supermips/blob/master/software/coremark/supermips/ee_printf.c

# Assumptions
All code executed by CEngine can be succesfully compiled with gcc.  
