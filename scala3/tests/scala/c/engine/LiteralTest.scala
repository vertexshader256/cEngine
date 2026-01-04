package scala.c.engine

class LiteralTest extends StandardTest {
	"a basic literal backslash test" should "print the correct results" in {
		val code =
			"""
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
		val code =
			"""
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
		val code =
			"""
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
		val code =
			"""
      void main() {
        unsigned long x = 4294967295UL; // max value of a unsigned long
				unsigned long y = 43423UL; // max value of a long long
				unsigned long j = 43423uL;
				unsigned long k = 43423Ul;
				unsigned long z = 43423ul;
				unsigned long z2 = 43423lu;
        printf("%ul %ul %ul %ul %ul %ul\n", x, y, j, k, z, z2);
      }"""

		checkResults(code)
	}

	"parsing unsigned long long" should "print the correct results" in {
		val code =
			"""
					int main() {
							unsigned long long x = 0x8000000000000000ULL;
							printf("%d", x);
							return 0;
					}"""

		checkResults(code)
	}

	"an unsigned long long suffix test" should "print the correct results" in {
		val code =
			"""
      void main() {
		 		unsigned long long j = 23452345ULL;
				unsigned long long k = 2342342ull;
				unsigned long long l = 2342342uLL;
				unsigned long long z = 2342342Ull;
			  unsigned long long z2 = 2342342uLL;
        printf("%ull %ull %ull %ull %ull\n", j, k, l, z, z2);
      }"""

		checkResults(code)
	}

	"an unsigned long long suffix test 2" should "print the correct results" in {
		val code =
			"""
				int main() {
						unsigned long long x = 0x800000045400000ULL;
						unsigned long long y = 0x800000045400000LLU;
						unsigned long long x2 = 0x800000045400000ull;
						unsigned long long y2 = 0x800000045400000llu;
						printf("%d %d %d %d", x, y, x2, y2);
						return 0;
				}"""
		checkResults(code)
	}

	"UTF-8 encoding test" should "print the correct results" in {
		val code =
			"""
			int main() {
					char *x = u8"test";
					printf("%s", x);
					return 0;
			}"""
		checkResults(code)
	}
}