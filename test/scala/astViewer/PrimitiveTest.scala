package scala.astViewer

class PrimitiveTest extends StandardTest {
//  "char test" should "print the correct results" in {
//    val code = """
//      void main() {
//        char x = 'd';
//        printf("%c\n", x);
//      }"""
//
//    checkResults(code)
//  }
  
  "hex test" should "print the correct results" in {
    val code = """
      void main() {
        // int x = 0xFFFFFFFF;   get this working
        int x = 0xABCDEF;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "short test" should "print the correct results" in {
    val code = """
      void main() {
        short x = 32767;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "short overflow test" should "print the correct results" in {
    val code = """
      void main() {
        short x = 1000000;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
}
