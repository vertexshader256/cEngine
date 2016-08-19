package scala.astViewer

class printf extends StandardTest {
    
  "Different basic prints" should "print the correct results" in {
    val code = """
      void main() {
        printf("Hello World!\n");
        printf("%s %s\n", "Hello", "World!");
        printf("%d\n", 1);
        printf("%s\n", "Hello World!");
      }"""

    checkResults(code)
  }
}
