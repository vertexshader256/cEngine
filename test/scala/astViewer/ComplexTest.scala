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
  
      void main()
      {
        printf("square root of %f\n",sq_root(9.0));
        printf("square root of %f\n",sq_root(3.0));       
        printf("square root of %f\n",sq_root(15.0));
        return 0;
      }
      """

    checkResults(code)
  }
}