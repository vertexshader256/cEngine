package scala.astViewer

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

  "function-scoped static vars" should "print the correct results" in {
    val code = """

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