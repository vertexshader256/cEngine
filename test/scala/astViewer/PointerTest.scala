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
  
  "A pointer with a unary expression" should "print the correct results" in {
    val code = """
      int z = 2;
      int *k = &z;
      void main() {
        (*k)++;
        printf("%d %d\n", *k, z);
        
      }"""
    
    checkResults(code)
    
    val code2 = """
      int z = 2;
      int *k = &z;
      void main() {
        (*k)--;
        printf("%d %d\n", *k, z);
        
      }"""
    
    checkResults(code2)
    
    val code3 = """
      int z = 2;
      int *k = &z;
      void main() {
        --(*k);
        printf("%d %d\n", *k, z);
        
      }"""
    
    checkResults(code3)
    
    val code4 = """
      int z = 2;
      int *k = &z;
      void main() {
        ++(*k);
        printf("%d %d\n", *k, z);
        
      }"""
    
    checkResults(code4)
  }
  
  "A simple pointer reassignment to another pointer" should "print the correct results" in {
    val code = """
      int x = 1;
      int z = 2;
      int *k = &z;
      int *y = &x;
      void main() {
        k = y;
        printf("%d %d\n", *y, *k);
        
      }"""
    
    checkResults(code)
  }
  
//  "A pointer as a function arg" should "print the correct results" in {
//    val code = """
//      void add(int *ptr) {
//        (*ptr)++;
//      }
//      
//      void main() {
//        int y = 10;
//        add(&y);
//        add(&y);
//        add(&y);
//        printf("%d\n", y);
//      }"""
//    
//    checkResults(code)
//  }
}