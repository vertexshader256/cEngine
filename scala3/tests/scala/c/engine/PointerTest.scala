package scala.c.engine

import scala.c.engine.models.NumBits
import scala.c.engine.models.NumBits.*

class StagingGround extends StandardTest {
	"assign then increment" should "print the correct results" in {
		val code =
			"""
      
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

class PointerSizeTest32 extends StandardTest2("32-bit pointer size",
	"""
      void main() {
        printf("%d\n", sizeof(void *));
      }"""
)

class PointerSizeTest64 extends StandardTest2("64-bit pointer size",
	"""
      void main() {
        printf("%d\n", sizeof(void *));
      }"""
) {
	override val numBits: NumBits = SixtyFourBits
}

class PointerTest extends StandardTest2("pointer equality",
	"""
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
)

class PointerTest2 extends StandardTest2("pointer typedef",
	"""
      typedef int* ptrType;

      void main() {
        int x = 10;
        ptrType y = &x;
        x++;

        printf("%d %d\n", *y, sizeof(ptrType));
        printf("%d\n", x);
      }"""
)

class PointerTest3 extends StandardTest2("arrays of strings",
	"""
      void main() {
        const char *alpha[2] = { "abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ"};
        printf("%s\n", alpha[0]);
        printf("%s\n", alpha[1]);
        printf("%c\n", alpha[0][0]);
        printf("%c\n", alpha[1][10]);
        //printf("%d\n", strlen(alpha[0]));
      }"""
)

class PointerTest4 extends StandardTest2("pointer indexing",
	"""
      void main() {
        char str[] = "Hello!\n";
        char *x = str + 2;
        char z = x[2];
        printf("%c\n", z);
      }"""
)

class PointerTest8 extends StandardTest2("pointers of all types",
	"""
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
)

class PointerTest9 extends StandardTest2("a double pointer being dereferenced",
	"""
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
)

class PointerTest10 extends StandardTest2("A pointer to a 2d array",
	"""
      extern char *x[];
      
      void main() {
        
        printf("%d\n", 5);
      }"""
)

class PointerTest11 extends StandardTest2("A simple pointer assignment",
	"""
      int x = 1;
      int *y = &x;
      void main() {
        printf("%d\n", *y);
      }"""
)

class PointerTest12 extends StandardTest2("Deferencing a casted address",
	"""
      float x = 9.74523f;

      void main() {
        int y = *(int*)&x;
        // include spaces in the pointer syntax
        printf("%d %d %d\n", *(int*)&x, *(int  *)&x, y);
      }"""
)

class PointerTest13 extends StandardTest2("A simple pointer reassignment",
	"""
      int x = 1;
      int *y = &x;
      int z = 10;
      void main() {
        y = &z;
        printf("%d\n", *y);
      }"""
)

class PointerTest14 extends StandardTest2("A function with a pointer as an argument",
	"""
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
)

class PointerTest15 extends StandardTest2("Saving a function arguments address",
	"""
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
)

class PointerTest16 extends StandardTest2("A function with a pointer to an unsized array as an argument",
	"""
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
)

class PointerTest17 extends StandardTest2("A simple pointer reassignment to another pointer",
	"""
      int x = 1;
      int z = 2;
      int *k = &z;
      int *y = &x;
      void main() {
        k = y;
        printf("%d %d\n", *y, *k);
        
      }"""
)

class PointerTest18 extends StandardTest2("A pointer with a unary expression",
	"""
      int z = 2;
      int *k = &z;
      void main() {
        (*k)++;
        int x = (*k)++;
        printf("%d %d %d\n", *k, z, x);
        
      }"""
)

class PointerTest19 extends StandardTest2("A pointer with a unary expression2",
	"""
      int z = 2;
      int *k = &z;
      void main() {
        (*k)--;
        printf("%d %d\n", *k, z);
        
      }"""
)

class PointerTest20 extends StandardTest2("A pointer with a unary expression4",
	"""
      int z = 2;
      int *k = &z;
      void main() {
        --(*k);
        printf("%d %d\n", *k, z);
        
      }"""
)

class PointerTest21 extends StandardTest2("A pointer with a unary expression5",
	"""
      int z = 2;
      int *k = &z;
      void main() {
        ++(*k);
        printf("%d %d\n", *k, z);
        
      }"""
)

class PointerTest23 extends StandardTest2("pointer with postincrement followed by subindex",
	"""
      void main() {
        char *x = "Hello!\n";
        printf("%c\n", x++[2]);
        printf("%c\n", x++[2]);
        printf("%c\n", x++[2]);
      }"""
)

class DoublePointer extends StandardTest2("basic double pointer use",
	"""
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
)

class DoublePointer2 extends StandardTest2("more double pointer use",
	"""
      void main() {
        int num = 45 , *ptr , **ptr2ptr, ***ptr3ptr;
        ptr     = &num;
        ptr2ptr = &ptr;
        ptr3ptr = &ptr2ptr;

        printf("%d\n", *ptr);
        printf("%d\n", **ptr2ptr);
        printf("%d\n", ***ptr3ptr);
      }"""
)