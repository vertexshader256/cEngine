package scala.astViewer

class ComplexTest extends StandardTest {
  "Order of operations test 3" should "print the correct results" in {
    val code = """

      double sq_root(double x)
      {
        double rt = 1, ort = 0;
        while(ort!=rt)
        {
          ort = rt;
          rt = ((x/rt) + rt) / 2;
        }
        return rt;
      }
  
      int main(void)
      {
        double i = 25.0;
        printf("square root of %d is %f\n",i, sq_root(i));
        return 0;
      }
      """

    checkResults(code)
  }
}