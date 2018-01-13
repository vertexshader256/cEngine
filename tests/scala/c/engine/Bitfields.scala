package scala.c.engine

class BitfieldStagingArea extends StandardTest {
  "A struct with bitfields" should "print the correct results" in {
      val code = """
     struct {
        unsigned int x : 1;
        unsigned int y : 1;
     } status2;

     int main( ) {
        printf( "Memory size occupied by status2 : %d\n", sizeof(status2));
        return 0;
     }"""

    checkResults(code)
  }

  "A struct with more bitfields" should "print the correct results" in {
    val code = """
     struct {
        unsigned int x : 4;
        unsigned int y : 4;
        unsigned int x2 : 4;
        unsigned int y2 : 4;
        unsigned int x3 : 4;
        unsigned int y3 : 4;
        unsigned int x4 : 4;
        unsigned int y4 : 4;
        unsigned int z : 1;
     } status2;

     int main( ) {
        printf( "Memory size occupied by status2 : %d\n", sizeof(status2));
        return 0;
     }"""

    checkResults(code)
  }

  "reading from a trivial bitfield" should "print the correct results" in {
    val code = """
     #include <stdio.h>

     struct test
     {
        unsigned int a: 8;
        unsigned int b: 8;
        unsigned int c: 8;
        unsigned int d: 8;
     };

     int main()
     {
        struct test dt;
        int i = 0xFF06FFFF;
        memcpy(&dt, &i, 4);
        printf("%d", dt.c);
        return 0;
     }"""

    checkResults(code)
  }

  "reading from a even less trivial bitfield" should "print the correct results" in {
    val code = """
     #include <stdio.h>

     struct test
     {
        unsigned int a: 1;
        unsigned int b: 7;
        unsigned int c: 9;
        unsigned int d: 8;
     };

     int main()
     {
        struct test dt;
        int i = 0x87A0A875;
        memcpy(&dt, &i, 4);
        printf("%d/%d/%d/%d", dt.a, dt.b, dt.c, dt.d);
        return 0;
     }"""

    checkResults(code)
  }

  "Initializing bit fields with init list" should "print the correct results" in {
    val code = """
     #include <stdio.h>

     // A space optimized representation of date
     struct test
     {
        unsigned int a: 2;
        unsigned int b: 2;
        unsigned int c: 2;
        unsigned int d: 2;
     };

     int main()
     {
        printf("Size of date is %d bytes\n", sizeof(struct test));
        struct test dt = {0, 1, 2, 3};
        dt.b = 3;
        printf("%d/%d/%d/%d", dt.a, dt.b, dt.c, dt.d);
        dt.d = 1;
        printf("%d/%d/%d/%d", dt.a, dt.b, dt.c, dt.d);
        return 0;
     }"""

    checkResults(code)
  }
}