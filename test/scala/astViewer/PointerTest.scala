package scala.astViewer

class PointerTest extends StandardTest {
  
  "A simple pointer assignment" should "print the correct results" in {
    val code = """
      int x = 1;
      int *y = &x;
      void main() {
        printf("%d\n", *y);
      }"""
    
    checkResults(code)
  }
  
  "A simple pointer reassignment" should "print the correct results" in {
    val code = """
      int x = 1;
      int *y = &x;
      int z = 10;
      void main() {
        y = &z;
        printf("%d\n", *y);
      }"""
    
    checkResults(code)
  }
  
  "A pointer as a function arg" should "print the correct results" in {
    val code = """
      void add(int *ptr) {
        *ptr++;
      }
      
      void main() {
        int y = 10;
        add(&y);
        add(&y);
        add(&y);
        printf("%d\n", y);
      }"""
    
    checkResults(code)
  }
}