package scala.c.engine

class ScopeTest extends StandardTest {
  "Two variables of the same name but different scope" should "print the correct results" in {
    val code = """

      void test()
      {
        int x = 10;
        printf("%d\n", x);
      }
  
      void main()
      {
        int x = 5;
        test();
        printf("%d\n", x);
      }
      """

    checkResults(code)
  }

  "shadowy escape" should "print the correct results" in {
    val code = """
      int x = 42;

      int func() {
        int x = 3840;
        {
          extern int x;
          return x;
        }
      }

      void main()
      {
        printf("%d\n", func(5));
      }
      """

    checkResults(code)
  }

  "scope of a block" should "print the correct results" in {
    val code = """

      void main()
      {
        int x = 5;
        {
           int x = 10;
           printf("%d\n", x);
        }
        printf("%d\n", x);
      }
      """

    checkResults(code)
  }

  "scope of a if statement" should "print the correct results" in {
    val code = """

      int x = 5;

      void main()
      {
        if (x == 5) {
           int x = 10;
           printf("%d\n", x);
        }
        printf("%d\n", x);
      }
      """

    checkResults(code)
  }

  "scope of a else statement" should "print the correct results" in {
    val code = """

      int x = 6;

      void main()
      {
        if (x == 5) {
           int x = 10;
           printf("%d\n", x);
        } else {
           int x = 10;
           printf("%d\n", x);
        }
        printf("%d\n", x);
      }
      """

    checkResults(code)
  }

  "scope of a for statement" should "print the correct results" in {
    val code = """

      int x = 6;

      void main()
      {
        int i = 0;
        for (i = 0; i < 5; i++) {
           int x = 10;
           printf("%d\n", x);
        }
        printf("%d\n", x);
      }
      """

    checkResults(code)
  }

  "scope of a while statement" should "print the correct results" in {
    val code = """

      int x = 6;

      void main()
      {
        int i = 0;
        while (i < 10) {
           int x = 10;
           printf("%d %d\n", x, i);
           i += 1;
        }
        printf("%d\n", x);
      }
      """

    checkResults(code)
  }

  "scope of a do while statement" should "print the correct results" in {
    val code = """

      int x = 6;

      void main()
      {
        int i = 0;
        do {
           int x = 10;
           printf("%d %d\n", x, i);
           i += 1;
        } while (i < 10);
        printf("%d\n", x);
      }
      """

    checkResults(code)
  }

  "function-scoped static vars" should "print the correct results" in {
    val code = """

      void test()
      {
        int y = 5;
        static int x = 10;
        x++;
        printf("%d\n", x);
      }

      void main()
      {
        test();
        test();
        test();
      }
      """

    checkResults(code)
  }

  "function-scoped static string vars" should "print the correct results" in {
    val code = """

      int test() {
        static char *metacharacters = "^$().[]*+?|\\Ssdbfnrtv";
        printf("%s\n", metacharacters);
        return 0;
      }

      void main()
      {
        test();
        test();
        test();
      }
      """

    checkResults(code)
  }



  "function-scoped unitialized static vars" should "print the correct results" in {
    val code = """

      void test(int z)
      {

        static int x;

        if (z == 5) {
           x += 6;
        }

        printf("%d\n", x);
      }

      void main()
      {
        test(5);
        test(6);
        test(5);
      }
      """

    checkResults(code)
  }

  "function-scoped static vars overriding global" should "print the correct results" in {
    val code = """

      int x = 45;

      void test()
      {
        static int x = 10;
        x++;
        printf("%d\n", x);
      }

      void main()
      {
        test();
        test();
        test();
      }
      """

    checkResults(code)
  }


}