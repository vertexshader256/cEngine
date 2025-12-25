package scala.c.engine

class StagingAreaPrimitive extends StandardTest2("bool test",
    """
      #include <stdbool.h>

      void main() {
        bool x = false;
        printf("%d\n", x);
      }"""
)

class RobustPrimitiveTest extends StandardTest2("robust primitive test", {

    val combo = List('a', 'b', 'e', 'f', 'g', 'h').combinations(2).toList
    val perms = combo.flatMap{case List(x,y) => List((x,y),(y,x))}
    val uniques = perms.toSet.toList

    """
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
})

class IntegerPromotions extends StandardTest2("Character promotion",
    // https://www.tutorialspoint.com/c_standard_library/limits_h.htm
    """

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
)

class SizeOfTest extends StandardTest2("A sizeof call on an different typed variables",
  """
      void main() {
        int x;
        double y;
        short z;
        char b;
        long c;
        long long d;
        float e;
        printf("%d %d %d %d %d %d %d\n", sizeof(x), sizeof(y), sizeof(z), sizeof(b), sizeof(c), sizeof(d), sizeof(e));
      }"""
)

class SizeOfTest2 extends StandardTest2("A variable self reference using sizeof",
 """
      void main() {
        int blah = sizeof(blah);
        int *blah2 = malloc(sizeof(*blah2));
        printf("%d %d\n", blah, sizeof(blah2));
      }"""
)

class SizeOfTest3 extends StandardTest2("A sizeof call on raw types",
  """
      void main() {
        printf("%d %d %d %d %d %d %d %d %d\n", sizeof(int), sizeof(double), sizeof(short),
                                      sizeof(float), sizeof(char), sizeof(long), sizeof(long long),
                                      sizeof(void), sizeof(void*));
      }"""
)

class SizeOfTest4 extends StandardTest2("A sizeof call on an array type",
    """
      void main() {
        int x[5];
        char y[5];
        long long z[5];
        printf("%d %d\n", sizeof(x), sizeof(y), sizeof(z));
      }"""
)

class SizeOfTest5 extends StandardTest2("A sizeof call on an array element",
    """
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
)

class SizeOfTest6 extends StandardTest2("A sizeof call on a pointer element",
    """
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
)

class SizeOfTest7 extends StandardTest2("A sizeof call on an array of shorts",
    """
      void main() {
        short x[5] = {1,2,3,4,5};
        printf("%d\n", sizeof(x) / sizeof(x[0]));
      }"""
)

class SizeOfTest8 extends StandardTest2("A sizeof call on a field",
    """
      void main() {
         struct regex_info {
           int brackets[100];
         };

         struct regex_info x;
         printf("%d\n", sizeof(x.brackets));
      }"""
)

class SizeOfTest9 extends StandardTest2("A sizeof call on a pointer to array variable",
    """
       int main()
       {
           int a[][3] = {1, 2, 3, 4, 5, 6};
           int (*ptr)[3] = a;
           printf("%d %d %d", sizeof(a), sizeof(ptr), sizeof(*ptr));
       }"""
)

class SizeOfTest10 extends StandardTest2("A sizeof on a 2d array variable",
    """
       int main()
       {
           int a[2][3] = {1, 2, 3, 4, 5, 6};
           printf("%d\n", sizeof(a));
           printf("%d\n", sizeof(a[0]));
       }"""
)

class SizeOfTest11 extends StandardTest2("A sizeof call on an uninitialized pointer to array variable 2",
    """
       int main()
       {
           int (*ptr)[3];
           printf("%d %d", sizeof(ptr), sizeof(*ptr));
       }"""
)

class SizeOfTest12 extends StandardTest2("array typedefs",
      """
      void main() {

         typedef int Test[10];

         Test x;
         x[1] = 10;
         printf("%d\n", x[1]);
         printf("%d\n", sizeof(Test));
         printf("%d\n", sizeof(x));
         printf("%d\n", sizeof(&x));
         printf("%d\n", sizeof(&x[1]));
         printf("%d\n", sizeof(++x[1]));
      }"""
)

class RolloverTest extends StandardTest2("char rollover test",
    """
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
)

class RolloverTest2 extends StandardTest2("short rollover test",
    """
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
)

class CharTest extends StandardTest2("char test",
    """
      void main() {
        char x = 'd';
        int y = 16;
        char z = y;
        char null = '\0';
        printf("%c %c\n", x, z);
        printf("%c\n", null);
        printf("%c %c %c\n", x, z, null);
      }"""
)

class HexTest extends StandardTest2("hex test",
    """
      void main() {
        int x = 0xFFFFFFFF;
        int i  = 0x5f3759df - ( x >> 1 );
        printf("%d %d\n", x, i);
      }"""
)

class ShortTest extends StandardTest2("short test",
    """
      void main() {
        short x = 32767;
        printf("%d\n", x);
      }"""
)

class ShortOverFlowTest extends StandardTest2("short overflow test",
    """
      void main() {
        short x = 1000000;
        printf("%d\n", x);
      }"""
)

class UnsignedCharTest extends StandardTest2("unsigned char test",
    """

      int test(unsigned char oneByte)
      {
        return oneByte;
      }

      void main()
      {
        printf("%d\n", test(176));
      }
      """
)

class PrimitiveTest5 extends StandardTest2("unsigned char array and clobbering test on unsigned types using unary expressions",
    """

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
      }
      """
)

class PrimitiveTest6 extends StandardTest2("char ptr initialized to string",
    """
      void main()
      {
        char *test = "TestString";
        printf("%s\n", test);
      }
      """
)

class PrimitiveTest7 extends StandardTest2("unsigned types as function arguments",
    """

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
      }
      """
)
