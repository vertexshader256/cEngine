package scala.astViewer

class StagingGround extends StandardTest {
 
}

class PointerTest extends StandardTest {
  
  "pointers of all types" should "print the correct results" in {
    val code = """

      void main() {
        char a = 1;
        short b = 1;
        int c = 1;
        long d = 1;
        float e = 1.0;
        double f = 1.0;
        
        char *a2;
        short *b2;
        int *c2;
        long *d2;
        float *e2;
        double *f2;
        
        a2 = &a;
        b2 = &b;
        c2 = &c;
        d2 = &d;
        e2 = &e;
        f2 = &f;
        printf("%d %d %d %d %f %f\n", *a2, *b2, *c2, *d2, *e2, *f2);
      }"""
    
    checkResults(code)
  }
  
  "A pointer to a 2d array" should "print the correct results" in {
    val code = """
      extern char *x[];
      
      void main() {
        
        printf("%d\n", 5);
      }"""
    
    checkResults(code)
  }
  
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
  
   "A function with a pointer as an argument" should "print the correct results" in {
    val code = """
      void add(int *x) {
        (*x)++;
      }
      
      // another way of incrementing
      void add2(int *x) {
        *x = *x + 1;
      }
      
      void main() {
        int y = 10;
        add(&y);
        add(&y);
        add2(&y);
        printf("%d\n", y);
      }"""
    
    checkResults(code)
  }
   
  "A function with a pointer to an unsized array as an argument" should "print the correct results" in {
    val code = """
      void add2(int *x, int y) {
        int i = 0;
        for(i = 0; i < y; i++) {
          printf("%d\n", x[i]);
        }
      }
      
      void main() {
        int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
        int n = sizeof a / sizeof a[0];
        add2(a, n);
      }"""
    
    checkResults(code)
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
  
  "some basic pointer arithmetic" should "print the correct results" in {
    val code = """
      void main() {
        char str[] = "Hello!\n";
        char *x = str + 1;
        printf("%s", x);
      }"""

    checkResults(code)
  }
  
  "some incremental pointer arithmetic" should "print the correct results" in {
    val code = """
      void main() {
        char str[] = "Hello!\n";
        char *x = str;
        printf("%s", x);
        x++;
        printf("%s", x);
        x++;
        x++;
        printf("%s", x);
        x--;
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