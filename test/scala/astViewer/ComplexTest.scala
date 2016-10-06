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
  
  
  "Fast inverse squart root test" should "print the correct results" in {
    val code = """float Q_rsqrt( float number )
    {
    	long i;
    	float x2, y;
    	const float threehalfs = 1.5F;
    
    	x2 = number * 0.5F;
    	y  = number;
    	i  = * ( int * ) &y;                       // evil floating point bit level hacking
    	i  = 0x5f3759df - ( i >> 1 );               // what the fuck? 
    	y  = * ( float * ) &i;
    	y  = y * ( threehalfs - ( x2 * y * y ) );   // 1st iteration
    
    	return y;
    }
    
    void main()
    {
      printf("%f\n", Q_rsqrt(9.0F)); // TODO: Dont require 'F' at end
      return 0;
    }
    """
    
    checkResults(code)
  }
  
  
  
//  "FNV1a test" should "print the correct results" in {
//    val code = """
//
//      const unsigned int Prime = 0x01000193; //   16777619
//      const unsigned int Seed  = 0x811C9DC5; // 2166136261
//
//      int fnv1a(unsigned char oneByte, int hash)
//      {
//        return (oneByte ^ hash) * Prime;
//      }
//  
//      void main()
//      {
//        printf("fnv1a result %d\n", fnv1a(10, Seed));
//        return 0;
//      }
//      """
//
//    checkResults(code)
//  }  
  

  
//  "DJB2 test" should "print the correct results" in {
//    val code = """
//
//      unsigned long
//      djb2(unsigned char *str)
//      {
//          unsigned long hash = 5381;
//          int c;
//      
//          while (c = *str++)
//              hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
//      
//          return hash;
//      }
//  
//      void main()
//      {
//        char *test = "TestString";
//        printf("djb2 result %d\n", djb2(test));
//        return 0;
//      }
//      """
//
//    checkResults(code)
//  }
}