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
  
  "nested struct type test with typedefs" should "print the correct results" in {
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
  
  "deeply nested struct type test with inner array" should "print the correct results" in {
    val code = """

      typedef struct {
        struct {
          int x;
          struct {
            struct {
              float y[5];
            } y;
            int x;
          } y;
        }
      } Test;
      
      void main() {
        Test x;
        x.x = 465;
        x.y.x = 234;
        x.y.y.y[4] = 5.4673f;
        printf("%d %d %f\n", x.x, x.y.x, x.y.y.y[4]);
      }"""

    checkResults(code)
  }
}
