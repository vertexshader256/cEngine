package scala.c.engine

class Literal extends StandardTest {
  "a basic literal backslash test" should "print the correct results" in {
    val code = """
      void main() {
        char *x = "jksjlhfds\\djjsdf\\";
        int i = 0;
        for (i = 0; i < strlen(x); i++) {
          if (x[i] == '\\') {
             printf("FOUND!\n");
          }
          printf("%c\n", x[i]);
        }
      }"""

    checkResults(code)
  }
}