# CEngine
Execute C code on the JVM

No bytecode, no JNI. Run C directly on the JVM.

Similiar to https://en.wikipedia.org/wiki/Nashorn_(JavaScript_engine)

Supports:
- Pointer aritmetic
- Multi-dimensional arrays
- Variable arguments
- Recursion
- Multilevel pointers
- Function pointers
- Unsigned types
- Memory allocation
- Standard functions

# Dependencies

Efforts have been made to reduce dependancies as much as possible.  Reflection is not used.

Eclipse CDT: https://eclipse.org/cdt/
JCPP: http://www.anarres.org/projects/jcpp/

# Test approach:
Standard output of CEngine must exactly match that of GCC.

CEngine was bootstrapped off of java.util.Formatter https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html but it can now execute an embedded version of printf, ee_printf.

ee_printf is found here:
https://github.com/bwalex/supermips/blob/master/software/coremark/supermips/ee_printf.c

Basic use case:
http://stackoverflow.com/questions/7865661/is-it-possible-to-run-c-source-code-from-java

# Assumptions
All code executed by CEngine can be succesfully compiled with gcc.  
