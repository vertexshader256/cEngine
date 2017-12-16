package scala.c.engine

class UnionTestStaging extends StandardTest {

  "union struct" should "print the correct results" in {
    val code = """

      union Test {
        int y;
        float x;
      } Test;

      void main() {
        union Test test;
        test.x = 334.283;
        printf("%f %d\n", test.x, test.y);
      }"""

    checkResults(code)
  }

  "union struct typedef" should "print the correct results" in {
    val code = """

      typedef union Test {
        int y;
        float x;
      } Test;

      void main() {
        Test test;
        test.x = -2373749;
        printf("%f %d\n", test.x, test.y);
      }"""

    checkResults(code)
  }

  "union struct pointer" should "print the correct results" in {
    val code = """

      typedef union Test {
        int y;
        float x;
      } Test;

      void main() {
        Test test;
        Test *test2 = &test;
        test.x = -2373749;
        printf("%f %d\n", test2->x, test2->y);
      }"""

    checkResults(code)
  }

  "embedded union struct pointer" should "print the correct results" in {
    val code = """

      typedef struct embedded
      {
        unsigned char type;
        union
        {
          int y;
          float x;
        } info;
      } embedded;

      void main() {
        embedded test;
        test.type = 'b';
        test.info.y = 1162651022;
        printf("%c %d %f\n", test.type, test.info.y, test.info.x);
      }"""

    checkResults(code)
  }





}