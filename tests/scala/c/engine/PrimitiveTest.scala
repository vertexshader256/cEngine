package scala.c.engine

class StagingAreaPrimitive extends StandardTest {
  "bool test" should "print the correct results" in {
    val code = """
      #include <stdbool.h>
      
      void main() {
        bool x = false;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
}

class RobustPrimitiveTest extends StandardTest {
  "robust primitive test" should "print the correct results" in {


    val combo = List('a', 'b', 'e', 'f', 'g', 'h').combinations(2).toList
    val perms = combo.flatMap{case List(x,y) => List((x,y),(y,x))}
    val uniques = perms.toSet.toList

    val code = """
       void main() {
        int x = 0;
        int a = 43;
        char b = 65;
        float c = 0.0f;
        double d = 0.0;
        long e = 254345;
        short f = 3544;
        unsigned int g = 776;
        unsigned short h = 345;

        """ + uniques.map{case (x,y) => s"""
              $x += $y;
              $x -= $y;
              $x *= $y;
              $x < $y;
              $x > $y;
              $x <= $y;
              $x >= $y;
              printf("%d\\n", $x);
             """
    }.reduce(_ + "\n" + _) ++ List('a', 'b', 'e', 'f').map{ x => s"""
              $x = 0;
              $x = $x + 2;
              $x = $x - 1;
              $x >>= 1;
              $x <<= 1;
              $x -= 5;
              $x += 2;

            printf("%d\\n", $x);"""
    }.reduce(_ + "\n" + _) ++ List('c', 'd').map{ x => s"""
            $x = 0;
            $x = $x + 2.0;
            $x = $x - 1.0;
            $x /= 2.0;
            $x *= 5.0;

            printf("%f\\n", $x);"""
    }.reduce(_ + "\n" + _) + "}"

    checkResults(code)
  }
}

class IntegerPromotions extends StandardTest {
  "Character promotion" should "print the correct results" in {
    // https://www.tutorialspoint.com/c_standard_library/limits_h.htm
    val code = """

      int main() {
         char cresult, c1, c2, c3;
         c1 = 100;
         c2 = 3;
         c3 = 4;
         cresult = c1 * c2 / c3;
         printf("%d\n", cresult);
         return(0);
      }
      """
    checkResults(code)
  }
}

class LimitsTest extends StandardTest {
  
  "A limits.h test" should "print the correct results" in {
    // https://www.tutorialspoint.com/c_standard_library/limits_h.htm
    val code = """
      #include <limits.h>
      
      int main() {
      
         printf("The number of bits in a byte %d\n", CHAR_BIT);
      
         printf("The minimum value of SIGNED CHAR = %d\n", SCHAR_MIN);
         printf("The maximum value of SIGNED CHAR = %d\n", SCHAR_MAX);
         printf("The maximum value of UNSIGNED CHAR = %d\n", UCHAR_MAX);
      
         printf("The minimum value of SHORT INT = %d\n", SHRT_MIN);
         printf("The maximum value of SHORT INT = %d\n", SHRT_MAX); 
      
         printf("The minimum value of INT = %d\n", INT_MIN);
         printf("The maximum value of INT = %d\n", INT_MAX);
      
         printf("The minimum value of CHAR = %d\n", CHAR_MIN);
         printf("The maximum value of CHAR = %d\n", CHAR_MAX);
      
         printf("The minimum value of LONG = %ld\n", LONG_MIN);
         printf("The maximum value of LONG = %ld\n", LONG_MAX);
        
         return(0);
      }
      """
    checkResults(code)
  }
}

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
        long long z[5];
        printf("%d %d\n", sizeof(x), sizeof(y), sizeof(z));
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
        long long g[5];
        
        printf("%d %d %d %d %d %d %d\n", sizeof(a[3]), sizeof(b[3]), sizeof(c[3]), sizeof(d[3]), sizeof(e[3]), sizeof(f[3]), sizeof(g[3]));
      }"""

    checkResults(code)
  }
  
  "A sizeof call on a pointer element" should "print the correct results" in {
    val code = """
      void main() {
        int *a;
        char *b;
        long *c;
        short *d;
        float *e;
        double *f;
        long long *g;
        
        printf("%d %d %d %d %d %d %d\n", sizeof(a[3]), sizeof(b[3]), sizeof(c[3]), sizeof(d[3]), sizeof(e[3]), sizeof(f[3]), sizeof(g[3]));
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
        char null = '\0';
        printf("%c %c\n", x, z);
        printf("%c\n", null);
        printf("%c %c %c\n", x, z, null);
      }"""

    checkResults(code)
  }
  
  "hex test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0xFFFFFFFF;
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

    checkResults(code, false)
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

  "unsigned char array and clobbering test on unsigned types using unary expressions" should "print the correct results" in {
    val code = """

      void main()
      {
        int i = 0;
        unsigned char *y = calloc(12, 1);
        unsigned char *x = y;
        y[2] = 'a';
        y[3] = 'b';
        y[11] = 'z';
        *x = 100;
        ++*x;
        ++x;
        ++*x;
        ++*x;
        ++x;
        *x = 100;
        ++*x;
        ++*x;

        for (i = 0; i < 12; i++) {
           putchar(*x);
           x++;
        }

        return 0;
      }
      """

    checkResults(code, false)
  }

  "char ptr initialized to string" should "print the correct results" in {
    val code = """
      void main()
      {
        char *test = "TestString";
        printf("%s\n", test);
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
