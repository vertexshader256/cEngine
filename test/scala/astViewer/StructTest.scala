package scala.astViewer

class StructTest extends StandardTest {
  "basic struct test" should "print the correct results" in {
    val code = """
      
      struct Test {
        int y;
        int z;
      };
      
      void main() {
        struct Test x;
        int y = 36;
        x.y = 465;
        printf("%d %d\n", x.y, y);
      }"""

    checkResults(code)
  }
  
}
