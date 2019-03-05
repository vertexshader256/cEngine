package scala.c.engine

class StagingGround extends StandardTest {
  "assign then increment" should "print the correct results" in {
    val code = """
      
      static int copy(char *buf, const char *fmt)
      {
        char *str;
        for (str = buf; *fmt; fmt++)
        {
            *str++ = *fmt;
            ///*str++ doesnt work!
        }
        

        //printf("%c\n", *fmt);
      }
      
      void main() {
        char test[10*10] = {0};
        char str[] = "Hello!";
        copy(test, str);
        printf("%s\n", test);
      }"""

    checkResults(code)
  }
}

class PointerSizeTest extends StandardTest {

  "32-bit pointer size" should "print the correct results" in {
    val code = """
      void main() {
        printf("%d\n", sizeof(void *));
      }"""

    checkResults(code, pointerSize = ThirtyTwoBits)
  }

  "64-bit pointer size" should "print the correct results" in {
    val code = """
      void main() {
        printf("%d\n", sizeof(void *));
      }"""

    checkResults(code, pointerSize = SixtyFourBits)
  }
}

class PointerArithmeticTest extends StandardTest {

  "pointer arithmetic on a pointer type" should "print the correct results" in {
    val code = """
      void main() {
        int num[10] = {1,2,3,4,5,6,7,8,9,10};
        int *arr = num;
        int y = 5;

        int *p1 = arr + 3;
        int *p2 = 5 + arr;
        int p3 = p2 - p1;
        int p4 = p1 - p2;
        int p5 = p1 + 5 - p2;

        int *p6 = arr + y;
        int *p7 = y + arr;

        printf("%d %d %d %d %d %d %d\n", *p1, *p2, p3, p4, p5, *p6, *p7);
      }"""

    checkResults(code)
  }

  "pointer arithmetic between pointers" should "print the correct results" in {
    val code = """
      void main() {
         int* x[5];
         int y[5];

         printf("%d %d\n", &x[0] - &x[1], &x[1] - &x[0]);
         printf("%d %d\n", &x[0] - &x[4], &x[4] - &x[0]);

         y[0] = 5;
         int* z = y;
         *(z + 1) = 10;
         printf("%d\n", y[1]);

      }"""

    checkResults(code)
  }

  "pointer arithmetic with pointers to structs" should "print the correct results" in {
    val code = """
      void main() {

         struct Test2 {
             int x;
             int y;
             int z;
         };

         struct Test {
            int x;
            int y;
            int z;
            struct Test* ptr2;
            struct Test2 j[10];
         };

         struct Test x[5];
         struct Test* ptr = &x[1];
         ptr->ptr2 = &x[2];
         int i = 1;

         printf("%d\n", &x[0] - &x[1]);
         printf("%d\n", &x[0] - (&x[1] + 1));
         printf("%d\n", &x[0] - (&x[1] - 1));
         printf("%d\n", &x[0] - (1 + &x[1]));

         printf("%d\n", &x[0] - ptr);
         printf("%d\n", &x[0] - (ptr + 1));
         printf("%d\n", &x[0] - (ptr - 1));
         printf("%d\n", &x[0] - (1 + ptr));

         printf("%d\n", x - ptr);
         printf("%d\n", x - (ptr++ + 1));
         printf("%d\n", x - (++ptr - 1));
         printf("%d\n", x - (1 + ptr));

         printf("%d\n", &x[2] - x);
         printf("%d\n", &x[2] - (x + 1));
         printf("%d\n", &x[2] - (x - 1));
         printf("%d\n", &x[2] - (1 + x));

         printf("%d\n", ptr - x);
         printf("%d\n", ptr - (x + 1));
         printf("%d\n", ptr - (x - 1));
         printf("%d\n", ptr - (1 + x));

         printf("%d\n", ptr - (x + i));
         printf("%d\n", ptr - (x - i));
         printf("%d\n", ptr - (i + x));

         printf("%d\n", ptr + i - x);
         printf("%d\n", x + i - ++ptr);
         printf("%d\n", x + 2 - ptr);
      }"""

    checkResults(code)
  }

  "pointer arithmetic with pointers to typedef structs" should "print the correct results" in {
    val code = """
      void main() {

         typedef struct {
             int x;
             int y;
             int z;
         } Test2;

         typedef struct {
            int x;
            int y;
            int z;
            struct Test* ptr2;
            Test2 j[10];
         } Test;

         Test x[5];
         Test* ptr = &x[1];
         ptr->ptr2 = &x[2];
         int i = 1;

         printf("%d\n", &x[0] - &x[1]);
         printf("%d\n", &x[0] - (&x[1] + 1));
         printf("%d\n", &x[0] - (&x[1] - 1));
         printf("%d\n", &x[0] - (1 + &x[1]));

         printf("%d\n", &x[0] - ptr);
         printf("%d\n", &x[0] - (ptr + 1));
         printf("%d\n", &x[0] - (ptr - 1));
         printf("%d\n", &x[0] - (1 + ptr));

         printf("%d\n", x - ptr);
         printf("%d\n", x - (ptr++ + 1));
         printf("%d\n", x - (++ptr - 1));
         printf("%d\n", x - (1 + ptr));

         printf("%d\n", &x[2] - x);
         printf("%d\n", &x[2] - (x + 1));
         printf("%d\n", &x[2] - (x - 1));
         printf("%d\n", &x[2] - (1 + x));

         printf("%d\n", ptr - x);
         printf("%d\n", ptr - (x + 1));
         printf("%d\n", ptr - (x - 1));
         printf("%d\n", ptr - (1 + x));

         printf("%d\n", ptr - (x + i));
         printf("%d\n", ptr - (x - i));
         printf("%d\n", ptr - (i + x));

         printf("%d\n", ptr + i - x);
         printf("%d\n", x + i - ++ptr);
         printf("%d\n", x + 2 - ptr);
      }"""

    checkResults(code)
  }

  "pointer arithmetic with pointers to arrays" should "print the correct results" in {
    val code = """
      void main() {

         typedef int Test[10];

         Test x[5];
         Test* ptr = &x[1];
         int i = 1;

         printf("%d\n", &x[0] - &x[1]);
         printf("%d\n", &x[0] - (&x[1] + 1));
         printf("%d\n", &x[0] - (&x[1] - 1));
         printf("%d\n", &x[0] - (1 + &x[1]));

         printf("%d\n", &x[0] - ptr);
         printf("%d\n", &x[0] - (ptr + 1));
         printf("%d\n", &x[0] - (ptr - 1));
         printf("%d\n", &x[0] - (1 + ptr));

         printf("%d\n", x - ptr);
         printf("%d\n", x - (ptr++ + 1));
         printf("%d\n", x - (++ptr - 1));
         printf("%d\n", x - (1 + ptr));

         printf("%d\n", &x[2] - x);
         printf("%d\n", &x[2] - (x + 1));
         printf("%d\n", &x[2] - (x - 1));
         printf("%d\n", &x[2] - (1 + x));

         printf("%d\n", ptr - x);
         printf("%d\n", ptr - (x + 1));
         printf("%d\n", ptr - (x - 1));
         printf("%d\n", ptr - (1 + x));

         printf("%d\n", ptr - (x + i));
         printf("%d\n", ptr - (x - i));
         printf("%d\n", ptr - (i + x));

         printf("%d\n", ptr + i - x);
         printf("%d\n", x + i - ++ptr);
         printf("%d\n", x + 2 - ptr);
      }"""

    checkResults(code)
  }

  "tricky pointer arithmetic with pointers case" should "print the correct results" in {
    val code = """
      void main() {

         typedef int Test[10];

         Test x[5];
         Test* ptr = &x[1];

         printf("%d\n", x - ++ptr);

      }"""

    checkResults(code)
  }

  "tricky pointer arithmetic with pointers case 2" should "print the correct results" in {
    val code = """
      void main() {
         int x[10][5];
         int (*ptr)[5] = &x[1];

         printf("%d\n", x - ++ptr);

      }"""

    checkResults(code)
  }

  "advanced pointer arithmetic" should "print the correct results" in {
    val code = """

       char *c[] = {"GeksQuiz", "MCQ", "TEST", "QUIZ"};
       char **cp[] = {c+3, c+2, c+1, c};
       char ***cpp = cp;

       int main()
       {
           printf("%s ", **++cpp);
           printf("%s ", *--*++cpp+3);
           printf("%s ", *cpp[-2]+3);
           printf("%s ", cpp[-1][-1]+1);
           return 0;
       }"""

    checkResults(code)
  }

  "advanced pointer arithmetic 2" should "print the correct results" in {
    val code = """

       int main()
       {
           int a[][3] = {1, 2, 3, 4, 5, 6};
           int (*ptr)[3] = a;
           printf("%d %d\n", (*ptr)[1], (*ptr)[2]);
           ++ptr;
           printf("%d %d\n", (*ptr)[1], (*ptr)[2]);
       }"""

    checkResults(code)
  }

  "advanced pointer arithmetic 5" should "print the correct results" in {
    val code = """

       int main()
       {
           int a[2][3] = {1, 2, 3, 4, 5, 6};
           int (*ptr)[3] = a;
           printf("%d %d\n", (*ptr)[1], (*ptr)[2]);
           ++ptr;
           printf("%d %d\n", (*ptr)[1], (*ptr)[2]);
       }"""

    checkResults(code)
  }

  "advanced pointer arithmetic 3" should "print the correct results" in {
    val code = """

       int fun(int arr[]) {
          arr = arr+1;
          printf("%d ", arr[0]);
       }
       int main(void) {
          int arr[2] = {10, 20};
          fun(arr);
          printf("%d", arr[0]);
          return 0;
       }"""

    checkResults(code)
  }
}

class PointerTest extends StandardTest {
  "pointer equality" should "print the correct results" in {
    val code = """

      int *testFcn() {
        return 0;
      }

      void main() {
        int x = 10;
        int x2 = 12;
        int *ptr[10];
        ptr[0] = &x;
        ptr[1] = 0;

        printf("%d\n", ptr[0] == &x);
        printf("%d\n", testFcn() == 0);
      }"""

    checkResults(code)
  }

  "pointer typedef" should "print the correct results" in {
    val code = """

      typedef int* ptrType;

      void main() {
        int x = 10;
        ptrType y = &x;
        x++;

        printf("%d %d\n", *y, sizeof(ptrType));
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "arrays of strings" should "print the correct results" in {
    val code = """
      void main() {
        const char *alpha[2] = { "abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ"};
        printf("%s\n", alpha[0]);
        printf("%s\n", alpha[1]);
        printf("%c\n", alpha[0][0]);
        printf("%c\n", alpha[1][10]);
        //printf("%d\n", strlen(alpha[0]));
      }"""

    checkResults(code)
  }
  
   "pointer indexing" should "print the correct results" in {
    val code = """
      void main() {
        char str[] = "Hello!\n";
        char *x = str + 2;
        char z = x[2];
        printf("%c\n", z);
      }"""

    checkResults(code)
  }
  
  "some basic pointer arithmetic/indexing" should "print the correct results" in {
    val code = """
      void main() {
        unsigned char *str = calloc(12,1);
        memcpy(str, "Hello!\n", 6);
        char *x = str + 2;
        char y = str[2];
        printf("%d\n", *x == y);
        printf("%s\n", x);
        *x++;
//        str++;
//        str++;
//        ++str;
//        str--;
        printf("%s\n", str);
        printf("%s\n", x);
      }"""

    checkResults(code)
  }


  
 "some basic pointer arithmetic" should "print the correct results" in {
    val code = """
      void main() {
        char str[] = "Hello!\n";
        char *x = str + 1;
        printf("%s\n", x);
        *x++;
        printf("%s\n", x);
      }"""

    checkResults(code)
  }

  "some basic pointer arithmetic 2" should "print the correct results" in {
    val code = """
    void main() {
      char str[] = "Hello!\n";
      char *x = str + 1;

      switch (x++[0]) {
         case 'H': printf("H\n"); break;
         case 'e': printf("e\n"); break;
         case 'l': printf("l\n"); break;
         case 'o': printf("o\n"); break;
      }

      printf("DONE\n");
    }"""

    checkResults(code)
  }
  
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
  
  "a double pointer being dereferenced" should "print the correct results" in {
   val code = """
     
     struct Test {
        int y;
        int z;
      };
     
     struct Test *head = 0;
     struct Test base = {483,2342};
     
     void test(struct Test **intPtr) {
       printf("%d %d\n", (*intPtr)->y, (*intPtr)->z);
       *intPtr = &base;
     }
     
     void main() {
       struct Test x = {24,54};
       head = &x;
       test(&head);
       //printf("%d %d\n", head->y, head->z);
     }  
  """
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

  "Saving a function arguments address" should "print the correct results" in {
    val code = """

      int *ptr = 0;
      void add(int *x) {
        ptr = &x[0];
      }

      void main() {
        int y[] = {1,2,3,4,5,6,7,8,9};
        int *z = y;
        add(z);
        printf("%d\n", *ptr);
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
  
  "A pointer with a unary expression" should "print the correct results" in {
    val code = """
      int z = 2;
      int *k = &z;
      void main() {
        (*k)++;
        int x = (*k)++;
        printf("%d %d %d\n", *k, z, x);
        
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

  "pointer with postincrement followed by subindex" should "print the correct results" in {
    val code = """
      void main() {
        char *x = "Hello!\n";
        printf("%c\n", x++[2]);
        printf("%c\n", x++[2]);
        printf("%c\n", x++[2]);
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