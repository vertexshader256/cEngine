package scala.c.engine

class CastTest extends StandardTest {
	"A simple cast" should "print the correct results" in {
		val code =
			"""

      long testLong() {
        return 1543543;
      }

      void main() {
        printf("%f\n", (double)testLong());
      }"""

		checkResults(code)
	}

	"A simple cast from a bin expr" should "print the correct results" in {
		val code =
			"""

      long testLong() {
        return 1543543;
      }

      void main() {
        short x = 5;
        short y = 5;

        printf("%f\n", (double)(82384238328 + 1));
        printf("%f\n", (double)(x + y));
        printf("%d\n", (long)(1.1 + 1.5));
        printf("%d\n", (long)(1.1f + 1.5f));
      }"""

		checkResults(code)
	}

	"A simple cast from a function call" should "print the correct results" in {
		val code =
			"""

      long testLong() {
        return 1543543;
      }

      void main() {
        printf("%f\n", (double)testLong());
      }"""

		checkResults(code)
	}

	"Robust casting for code coverage" should "print the correct results" in {
		val code =
			"""

      void main() {
			  double d = 1.45;
		    float f = 1.87;
		    short s = 23;
			  long l = 65;
		    long long l2 = 16;
		    char c = 'c';

	 			long long blah = (long long)d;
		    long long blah2 = (long long)f;
			  int blah3 = (int)d;
		    int blah4 = (int)f;
			  short blah5 = (short)s;
		    char blah6 = (char)c;
			  float blah7 = (float)l;
		    double blah8 = (double)f;
			  double blah9 = (float)l2;

        printf("%d %d %f %f\n", blah, blah2, d, f);
				printf("%d %d %d\n", blah3, blah4, blah5);
		    printf("%d %f %f %f\n", blah6, blah7, blah8, blah9);

      }"""

		checkResults(code)
	}
}