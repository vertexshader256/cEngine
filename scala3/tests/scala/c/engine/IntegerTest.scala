package scala.c.engine

class IntegerTest extends StandardTest {
	"bitwise ops on unsigned long longs" should "print the correct results" in {
		val code =
			"""
				int main() {
					union {
							double dbl_val;
							long long int_val;
					} u;

					u.dbl_val = -5.12345;
					printf("before: %f\n", u.dbl_val);
					u.int_val |= 0x8000000000000000ULL;
					printf("after: %f\n", u.dbl_val);

					return 0;
				}"""

		checkResults(code)
	}

	"more bitwise ops on unsigned long longs" should "print the correct results" in {
		val code =
			"""
					int main() {
						unsigned long long x = 0xBFFFFFFFFFFFFFFFULL;
						printf("before: %d\n", x);
						x &= 0xBFFFFFFFFFFFFFFFULL;
						printf("after: %f\n", x);

						return 0;
					}"""

		checkResults(code)
	}
}
