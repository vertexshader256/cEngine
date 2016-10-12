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
  
  "Deferencing a casted address" should "print the correct results" in {
    val code = """
      float x = 9.74523f;

      void main() {
        int y = *(int*)&x;
        // include spaces in the pointer syntax
        printf("%d %d %d\n", *(int*)&x, *(int  *)&x, y);
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
  
  "A pointer as a function arg" should "print the correct results" in {
    val code = """
      void add(int *ptr) {
        (*ptr)++;
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
  
  "some basic pointer arithmetic" should "print the correct results" in {
    val code = """
      void main() {
        char str[] = "Hello!\n";
        char *x = str + 1;
        printf("%s", x);
      }"""

    checkResults(code)
  }
  
  "some more advanced pointer arithmetic" should "print the correct results" in {
    val code = """
      void main() {
        int arr[10] = {1,2,3,4,5,6,7,8,9,10};
        int *p1, *p2;
    
        p1 = arr + 3;
        p2 = p1 - 2;
        printf("%d %d", *p1, *p2);
      }"""

    checkResults(code)
  }
}

class DoublePointer extends StandardTest {

  "basic double pointer use" should "print the correct results" in {
    val code = """
      
      void MoveToNextElement(char** i) {
         (*i)++;
      }
      
      void main() {
        char string[] = "epic";
        char *ptr = string;
        printf("%c\n", *ptr);
        MoveToNextElement(&ptr);
        printf("%c\n", *ptr);
      }"""

    checkResults(code)
  }
  
  "more double pointer use" should "print the correct results" in {
    val code = """
      
      void main() {
        int num = 45 , *ptr , **ptr2ptr, ***ptr3ptr;
        ptr     = &num;
        ptr2ptr = &ptr;
        ptr3ptr = &ptr2ptr;

        printf("%d\n", *ptr);
        printf("%d\n", **ptr2ptr);
        printf("%d\n", ***ptr3ptr);
      }"""

    checkResults(code)
  }
}