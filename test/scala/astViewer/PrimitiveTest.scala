package scala.astViewer

class SizeOfTest extends StandardTest {

  "A sizeof call on an int var" should "print the correct results" in {
    val code = """
      void main() {
        int x;
        double y;
        printf("%d %d\n", sizeof(x), sizeof(y));
      }"""

    checkResults(code)
  }
  
  "A sizeof call on an int type" should "print the correct results" in {
    val code = """
      void main() {
        printf("%d %d %d %d\n", sizeof(int), sizeof(double), sizeof(float), sizeof(char));
      }"""

    checkResults(code)
  }
  
  "A sizeof call on an array type" should "print the correct results" in {
    val code = """
      void main() {
        int x[5];
        char y[5];
        printf("%d %d\n", sizeof(x), sizeof(y));
      }"""

    checkResults(code)
  }
  
  "A sizeof call on an array element" should "print the correct results" in {
    val code = """
      void main() {
        int a[5];
        char b[5];
        long c[5];
        short d[5];
        float e[5];
        double f[5];
        
        printf("%d %d\n", sizeof(a[3]), sizeof(b[3]), sizeof(c[3]), sizeof(d[3]), sizeof(e[3]), sizeof(f[3]));
      }"""

    checkResults(code)
  }
}

class RolloverTest extends StandardTest {
  "char rollover test" should "print the correct results" in {
    val code = """
      void main() {
        char x = 128;
        char xplusone = 128 + 1;
        unsigned char y = 255;
        unsigned char yplusone = y + 1;
        int x2 = x;
        int y2 = y;
        int x3 = xplusone;
        int y3 = yplusone;
        printf("%d %d %d %d\n", x2, y2, y3, x3);
      }"""

    checkResults(code)
  }
  
  "short rollover test" should "print the correct results" in {
    val code = """
      void main() {
        short x = 32767;
        short xplusone = 32767 + 1;
        unsigned short y = 65535;
        unsigned short yplusone = y + 1;
        int x2 = x;
        int y2 = y;
        int y3 = yplusone;
        int x3 = xplusone;
        printf("%d %d %d %d\n", x2, y2, y3, x3);
      }"""

    checkResults(code)
  }
}

class PrimitiveTest extends StandardTest {
  "char test" should "print the correct results" in {
    val code = """
      void main() {
        char x = 'd';
        int y = 16;
        char z = y;
        printf("%c %c\n", x, z);
      }"""

    checkResults(code)
  }
  
  "hex test" should "print the correct results" in {
    val code = """
      void main() {
        // int x = 0xFFFFFFFF;   get this working
        int x = 0xABCDEF;
        int i  = 0x5f3759df - ( x >> 1 );
        printf("%d %d\n", x, i);
      }"""

    checkResults(code)
  }
  
  "short test" should "print the correct results" in {
    val code = """
      void main() {
        short x = 32767;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "short overflow test" should "print the correct results" in {
    val code = """
      void main() {
        short x = 1000000;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "unsigned int prints from hex" should "print the correct results" in {
    val code = """

      const unsigned int prime = 0x01000193; //   16777619
      const unsigned int seed  = 0x811C9DC5; // 2166136261

      void main()
      {
        printf("%d %d\n", prime, seed);
        return 0;
      }
      """

    checkResults(code)
  } 
  
  "unsigned char test" should "print the correct results" in {
    val code = """

      int test(unsigned char oneByte)
      {
        return oneByte;
      }
  
      void main()
      {
        printf("%d\n", test(176));
        return 0;
      }
      """

    checkResults(code)
  } 
  
  "unsigned types as function arguments" should "print the correct results" in {
    val code = """

      int intTest(unsigned int data)
      {
        return data;
      }
      
      int shortTest(unsigned short data)
      {
        return data;
      }
           
      short shortTest2(unsigned short data)
      {
        return data;
      }
  
      void main()
      {
        printf("%d %d %d\n", intTest(4294967241), shortTest(4294967241), shortTest2(38233));
        return 0;
      }
      """

    checkResults(code)
  } 
}
