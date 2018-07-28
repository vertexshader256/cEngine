package scala.c.engine

class StructTestStaging extends StandardTest {


  "pointer to struct with array member" should "print the correct results" in {
    val code = """

      struct Test {
        int y;
        int x;
        int length;
        int *z;
        struct Test *next;
      };

      struct Test *head = 0;

      void main() {
        int i = 0;
        struct Test x;
         x.y = 343;
         x.x = 543;
        struct Test *y = 0;
        int blah[] = {1,2,3,4,5,6,7,8,9,0};

        y = &x;
        y->z = malloc(40);
         memcpy(y->z, blah, 40);
         for (i = 0; i < 10; i++) {
           printf("%d\n", y->z[i]);
         }
        y->next = head;
        y->y = 465;
        printf("%d %d %d\n", x.y, y->y, y->next);
      }"""

    checkResults(code)
  }
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

  "advanced struct init" should "print the correct results" in {
    val code = """

     typedef struct {
         char *name;
         int weight;
         int value;
         int count;
       } item_t;

       item_t items[] = {
         {"map",                      9,   150,   1},
         {"compass",                 13,    35,   1},
         {"water",                  153,   200,   2},
         {"sandwich",                50,    60,   2},
         {"glucose",                 15,    60,   2},
         {"tin",                     68,    45,   3},
         {"banana",                  27,    60,   3},
         {"apple",                   39,    40,   3},
         {"cheese",                  23,    30,   1},
         {"beer",                    52,    10,   3},
         {"suntan cream",            11,    70,   1},
         {"camera",                  32,    30,   1},
         {"T-shirt",                 24,    15,   2},
         {"trousers",                48,    10,   2},
         {"umbrella",                73,    40,   1},
         {"waterproof trousers",     42,    70,   1},
         {"waterproof overclothes",  43,    75,   1},
         {"note-case",               22,    80,   1},
         {"sunglasses",               7,    20,   1},
         {"towel",                   18,    12,   2},
         {"socks",                    4,    50,   1},
         {"book",                    30,    10,   2},
       };

      void main() {
        printf("%d\n", items[0].weight);
        printf("%s\n", items[0].name);
      }"""

    checkResults(code)
  }

  "indexing with a variable" should "print the correct results" in {
    val code = """

      struct Test {
        int* data;
        int length;
      };

      void main() {
        struct Test a = {0};
        a.data = malloc(12);
        a.data[2] = 10;
        a.length = 2;
        printf("%d\n", a.data[a.length]);
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
  
  "struct with function pointer field" should "print the correct results" in {
    val code = """
      #include <stdlib.h>

      struct Test {
        int (*testFcn)(int);
        void (*testFcn2)(void*);
      };
      
      int what(int x) {
         return x + 10;
      }
      
      void setFcn(struct Test *test) {
         test->testFcn = what;
         test->testFcn2 = free;
      }
      
      void passStruct(struct Test *test) {
         printf("%d\n", test->testFcn('d'));
         test->testFcn2(10);
      }
      
      void main() {
        struct Test a = {0};
        struct Test *b = &a;
        setFcn(b);
        printf("%d %d\n", a.testFcn(4), b->testFcn(57));
        passStruct(b);
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

  "advanced struct sizeof test" should "print the correct results" in {
    val code = """

        struct slre_cap {
           const char *ptr;
           int len;
         };

        struct bracket_pair {
         const char *ptr;
         int len;
         int branches;
         int num_branches;
       };

       struct branch {
         int bracket_index;
         const char *schlong;
       };

      struct Test {
         struct bracket_pair brackets[24];
         int num_brackets;
         struct branch branches[24];
         int num_branches;
         int num_caps;
         int flags;
       };

      void main() {

        struct Test test;

        printf("%d\n", sizeof(struct Test));
        printf("%d\n", sizeof(struct branch));
        printf("%d\n", sizeof(struct bracket_pair));
        printf("%d\n", sizeof(struct slre_cap));

        printf("%d\n", sizeof(test.brackets));
        printf("%d\n", sizeof(test.branches));
        printf("%d\n", sizeof(test.num_branches));
        printf("%d\n", sizeof(test.flags));
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

  "struct field offset test" should "not clobber" in {
    val code = """

     #include <stddef.h>
     #include <stdio.h>
     #include <stdlib.h>

     struct bracket_pair {
       const char *ptr;
       int len;
       int branches;
       int num_branches;
     };

     struct branch {
       int bracket_index;
       const char *schlong;
     };

      struct Test {
        struct bracket_pair brackets[30];
        int num_brackets;
        struct branch branches[30];
        int num_branches;
      };

      void main() {
        printf("%d %d %d %d\n", offsetof(struct Test, brackets),
                                offsetof(struct Test, num_brackets),
                                offsetof(struct Test, branches),
                                offsetof(struct Test, num_branches));
      }"""

    checkResults(code, true)
  }

  "struct field clobbering test" should "not clobber" in {
    val code = """
      struct bracket_pair {
        const char *ptr;
      };

      struct branch {
        int bracket_index;
      };

      struct Test {
        struct bracket_pair brackets[2];
        struct branch branches[2];
      };

      void main() {
        struct Test x;
        char string[] = "hello";
        x.brackets[0].ptr = string;
        x.branches[0].bracket_index = 50;

        printf("%d\n", x.brackets[0].ptr == string);
      }"""

    checkResults(code, true)
  }


  
  
}
