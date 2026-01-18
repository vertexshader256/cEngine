package scala.c.engine

class PrintTest extends StandardTest {
	"two sequential calls to printf with padding spaces" should "print the correct results" in {
		val code =
			"""
				int main(int argc, char*argv[])
				{
						printf("  ");
						printf("%d", 5);
						return 0;
				}
			"""

		checkResults(code)
	}
}

class RobustPrint extends StandardTest {

	"printing a unsigned long long with u format" should "print the correct results" in {
		val code =
			"""
				int main() {
					unsigned long long x = 0xFFFFFFFFFFFFFFFF;
					unsigned long long x2 = 0xFFFFFFFFFFFFFFFE;
					printf("after: %u %u\n", x, x2);
					return 0;
				}"""

		checkResults(code)
	}

	"snprintf test" should "print the correct results" in {
		val code =
			"""
				void main() {
					char result[100] = {0};
					int x = 100;
					snprintf(result, 100, "after: %d\n", x);
					printf("results: %s", result);
				}"""

		checkResults(code)
	}

	"printing a long as a char" should "print the correct results" in {
		val code =
			"""
				void main() {
					unsigned long long x = 35;
			    short y = 40;
			    long long z = 45;
			    unsigned int zz = 50;
			    unsigned short zzz = 55;
					printf("%c\n", x);
			    printf("%c\n", y);
			    printf("%c\n", z);
			    printf("%c\n", zz);
			    printf("%c\n", zzz);
				}"""

		checkResults(code)
	}

	"printing a unsigned long long with llu format" should "print the correct results" in {
		val code =
			"""
					void main() {
						unsigned long long x = 0xFFFFFFFFFFFFFFFFULL;
						printf("%llu\n", x);
					}"""

		checkResults(code)
	}

	"printing dynamic width string" should "print the correct results" in {
		val code =
			"""
					void main() {
						printf("[%*s]\n", 5, "Hi");
					}"""

		checkResults(code)
	}

	"printing a unsigned long long with hex format" should "print the correct results" in {
		val code =
			"""
				void main() {
					unsigned long long x = 0xFFFFFFFFFFFFFFFFULL;
					printf("%x %X %#x %#X\n", x, x, x, x);
				}"""

		checkResults(code)
	}

	"printing a pointer address" should "print the correct results" in {
		val code =
			"""
				void main() {
					int x = 123;
					int y = 234;
					int* xp = &x;
					int* yp = &y;
					printf("%p %p\n", x, y);
				}"""

		checkResults(code)
	}
	
	"printing long as float" should "print the correct results" in {
		val code =
			"""#include <stdio.h>

			int main()
			{
				int integer_value = 0xFFFFFFFF;

				printf("As a float: %f\n", (float)integer_value);
				printf("As a formatted float: %.2f\n", (float)integer_value);

				return 0;
			}"""

		checkResults(code)
	}
}
