# CEngine
Execute C code on the JVM

Lets not mess with generating JVM bytecode.  Forget the JNI.  Run C directly on the JVM.

Similiar to https://en.wikipedia.org/wiki/Nashorn_(JavaScript_engine) but C instead of JS.

Supports:
- Multi-dimensional arrays
- Variable arguments
- Recursion
- Multilevel pointers
- Function pointers
- All primitive types
- Unsigned primitives
- Memory allocation
- Some standard functions

Basic use case:
http://stackoverflow.com/questions/7865661/is-it-possible-to-run-c-source-code-from-java

Honorable mentions: JPC
https://github.com/ianopolous/JPC
