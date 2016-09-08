package scala.astViewer

class TypesTest extends StandardTest {
  "Simple type test" should "print the correct results" in {
    val code = """

      typedef int customType;

      void main() {
      
        customType x;
        x = 5;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
}
