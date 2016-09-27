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
  
  "struct type test" should "print the correct results" in {
    val code = """

      typedef struct {
        int y;
        int z;
      } Test;
      
      void main() {
        Test x;
        x.y = 465;
        x.z = 234;
        printf("%d %d\n", x.y, x.z);
      }"""

    checkResults(code)
  }
  
  "nested struct type test" should "print the correct results" in {
    val code = """

      typedef struct {
        int z;
      } Inner;

      typedef struct {
        int x;
        Inner y;
      } Outer;
      
      void main() {
        Outer x;
        x.x = 465;
        x.y.z = 234;
        printf("%d %d\n", x.x, x.y.z);
      }"""

    checkResults(code)
  }
}
