package scala.astViewer

class StructTestStaging extends StandardTest {
}

class StructTest extends StandardTest {
  
  "initializer list populating a pointer" should "print the correct results" in {
    val code = """
      
      struct Test {
        int y;
        int x;
        struct Test *next;
      };

      void main() {
        struct Test x = {343, 543, 0};
        printf("%d %d %d\n", x.y, x.y, x.next);
      }"""

    checkResults(code)
  }
  
  "pointer to struct" should "print the correct results" in {
    val code = """
      
      struct Test {
        int y;
        int x;
        struct Test *next;
      };
      
      struct Test *head = 0;
      
      void main() {
        struct Test x = {343, 543, 0};
        struct Test *y = head;
        y = &x;
        y->next = head;
        y->y = 465;
        printf("%d %d %d\n", x.y, y->y, y->next);
      }"""

    checkResults(code)
  }
  
  "structure pointer typedef" should "print the correct results" in {
    val code = """
      
      typedef struct linked_node* lnode_p;
      
      struct list{
      	lnode_p first;
      };
      
      struct linked_node{
      	void* data;
      };
      
      typedef struct list * list_p;

      void main() {
        struct list x = {4};
        struct linked_node test = {14};
        x.first = &test;

        lnode_p y = x.first;
        void *before = y->data;
        x.first = 0;
        void *after = y->data;
        printf("%d\n", before != after);
        
      }"""

    checkResults(code)
  }
  
  "function which takes struct pointer" should "print the correct results" in {
    val code = """
      
      struct Test {
        int y;
        int x;
        struct Test *next;
      };
      
      typedef struct TypedefType {
        int y;
        int x;
        struct Test *next;
      } TypedefType;
      
      void modify(struct Test *test) {
         test->x = 15;
         test->y = test->x + 55;
      }
      
      void modify2(TypedefType *test) {
         test->next->x = 15;
         test->next->y = test->x + 55;
      }
      
      void main() {
        struct Test x = {343, 543, 0};
        TypedefType y = {1, 40, &x};
        printf("%d %d\n", x.y, x.y);
        printf("%d %d\n", y.y, y.y);
        modify(&x);
        modify2(&y);
        printf("%d %d\n", x.y, x.y);
        printf("%d %d\n", y.next->y, y.next->y);
      }"""

    checkResults(code)
  }
  
  "binary expressions on field value" should "print the correct results" in {
    val code = """
      
      struct Test {
        int y;
        int z;
      };
      
      void main() {
        struct Test a = {1,2};
        printf("%d\n", a.y == 1);
      }"""

    checkResults(code)
  } 
  
  "struct ptr assignment" should "print the correct results" in {
    val code = """
      
      struct Test {
        int y;
        int z;
      };
      
      void main() {
        struct Test a = {1,2};
        struct Test *x;
        struct Test *y = &a;
        struct Test *z = y;
        x = y;
        printf("%d %d %d %d\n", x->y, x->z, y->z, z->z);
      }"""

    checkResults(code)
  } 
  
  "struct initializer" should "print the correct results" in {
    val code = """
      
      struct Test {
        int one;
        double two;
        char three;
      };
      
      void main() {
        struct Test x = {1, 2.0, 'a'};
        printf("%d %f %c\n", x.one, x.two, x.three);
      }"""

    checkResults(code)
  }
  
  "basic struct test" should "print the correct results" in {
    val code = """
      
      struct Test {
        int y;
        int z;
      };
      
      void main() {
        struct Test x;
        int y = 36;
        x.y = 465;
        printf("%d %d\n", x.y, y);
      }"""

    checkResults(code)
  }
  
  "basic struct sizeof test" should "print the correct results" in {
    val code = """
      
      struct Test {
        int y;
        int z;
      };
      
      void main() {
        printf("%d\n", sizeof(struct Test));
      }"""

    checkResults(code)
  }
  
  
  
  "setting a structure pointer equal to a pointer" should "print the correct results" in {
    val code = """
      
      struct Test {
        int y;
        int x;
        struct Test *next;
      };
      
      struct Test *head = 0;
      
      void main() {
        struct Test x = {0,0,0};
        head = x.next;
        printf("%d\n", head);
      }"""

    checkResults(code)
  }
  
  "moderate struct test" should "print the correct results" in {
    val code = """
      
      struct Test {
        int y;
      };
      
      void main() {
        struct Test x;
        struct Test u;
        x.y = 465;
        u.y = 234;
        printf("%d %d\n", u.y, x.y);
      }"""

    checkResults(code)
  }
  
  "struct test multiple members" should "print the correct results" in {
    val code = """
      
      struct Test {
        int y;
        int z;
      };
      
      void main() {
        struct Test x;
        x.y = 465;
        x.z = 234;
        printf("%d %d\n", x.y, x.z);
      }"""

    checkResults(code)
  }
  
  
}
