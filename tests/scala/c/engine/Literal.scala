package scala.c.engine

class Literal extends StandardTest {
  "a basic literal backslash test" should "print the correct results" in {
    val code = """
      void main() {
        char *x = "tel:\\+(\\d+[\\d-]+\\d)";
        int i = 0;
        for (i = 0; i < strlen(x); i++) {
          if (x[i] == '\\') {
             printf("FOUND!\n");
          }
          printf("%c\n", x[i]);
        }
        printf("%s\n", x);
      }"""

    checkResults(code)
  }

	"an integer suffix test" should "print the correct results" in {
		val code = """
      void main() {
        long long x = 9223372036854775807LL; // max value of a long long
				long long y = 9223372036854775807ll; // max value of a long long
				long long j = 2147483647L; // max value of a long long
				long long k = 2147483647l; // max value of a long long
        printf("%lld %lld %ld %ld\n", x, y, j, k);
      }"""

		checkResults(code)
	}

	"an floating point suffix test" should "print the correct results" in {
		val code = """
      void main() {
        float x = 23425234.45234f; // max value of a long long
				float y = 43423.545345F; // max value of a long long
				double j = 456435345.324534543d; // max value of a long long
				double k = 24532423.42345453D; // max value of a long long
        printf("%f %f %f %f\n", x, y, j, k);
      }"""

		checkResults(code)
	}

	"an unsigned long suffix test" should "print the correct results" in {
		val code = """
      void main() {
        unsigned long x = 4294967295UL; // max value of a unsigned long
				unsigned long y = 43423UL; // max value of a long long
				unsigned long j = 43423uL;
				unsigned long k = 43423Ul;
				unsigned long z = 43423ul;
        printf("%ul %ul %ul %ul %ul\n", x, y, j, k, z);
      }"""

		checkResults(code)
	}

	// TODO: Try and figure out unsigned long long
//	"an unsigned long long suffix test" should "print the correct results" in {
//		val code = """
//      void main() {
//	 			unsigned long long j = 18446744073709551615ULL; // max value of a long long
//				unsigned long long k = 543353453452354354ull; // max value of a long long
//        printf("%ull %ull\n", j, k);
//      }"""
//
//		checkResults(code)
//	}
}