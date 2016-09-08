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
  
  "chained type test" should "print the correct results" in {
    val code = """

      typedef int customType;
      typedef customType customType2;
      typedef customType2 customType3;
      typedef customType3 customType4;

      void main() {
      
        customType4 x;
        x = 5;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
}
