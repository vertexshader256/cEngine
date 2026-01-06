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

	"A cast fron a negative short to an unsigned int" should "print the correct results" in {
		val code =
			"""
				void main() {
			    short x = -1;
			    unsigned int y = (unsigned int)x;
					printf("%d\n", y);
				}"""

		checkResults(code)
	}

	private def getCastingTest(theType: String, z: String) = {
		s"""
			void main() {
				char x = 'a';
				short x2 = -1;
		    int x3 = -435;
			  long x4 = -435;
		    long long x5 = -435;
			  float x6 = -21312.045;
		    double x7 = -3432.54534;
		    unsigned long long x8 = 0xFFFFFFFFFFFFFFFFULL;
				$theType y = ($theType)x;
				$theType y2 = ($theType)x2;
				$theType y3 = ($theType)x3;
				$theType y4 = ($theType)x4;
				$theType y5 = ($theType)x5;
				$theType y6 = ($theType)x6;
				$theType y7 = ($theType)x7;
		    $theType y8 = ($theType)x8;
				printf("%$z %$z %$z %$z %$z %$z %$z %$z\\n", y, y2, y3, y4, y5, y6, y7, y8);
			}
			""".stripMargin
	}

	"Casting various types to a long long" should "print the correct results" in {
		checkResults(getCastingTest("long long", "d"))
	}

	"Casting various types to a unsigned long long" should "print the correct results" in {
		checkResults(getCastingTest("unsigned long long", "d"))
	}

	"Casting various types to a short" should "print the correct results" in {
		checkResults(getCastingTest("short", "d"))
	}

	"Casting various types to an unsigned short" should "print the correct results" in {
		checkResults(getCastingTest("unsigned short", "d"))
	}

	"Casting various types to a char" should "print the correct results" in {
		checkResults(getCastingTest("char", "d"))
	}

	"Casting various types to an unsigned char" should "print the correct results" in {
		checkResults(getCastingTest("unsigned char", "d"))
	}

	"Casting various types to a long" should "print the correct results" in {
		checkResults(getCastingTest("long", "d"))
	}

	"Casting various types to an unsigned long" should "print the correct results" in {
		checkResults(getCastingTest("unsigned long", "d"))
	}

	"Casting various types to a int" should "print the correct results" in {
		checkResults(getCastingTest("int", "d"))
	}

	"Casting various types to an unsigned int" should "print the correct results" in {
		checkResults(getCastingTest("unsigned int", "d"))
	}

	//"Casting various types to a float" should "print the correct results" in {
	//	checkResults(getCastingTest("float", "f"))
	//}

	//"Casting various types to a double" should "print the correct results" in {
	//	checkResults(getCastingTest("double", "f"))
	//}

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