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
}