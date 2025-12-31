package scala.c.engine

class RobustFP extends StandardTest {
	"union to view floating point bits" should "print the correct results" in {
		val code =
			"""
				int main() {
						union {
								double dbl_val;
								long long int_val;
						} u;

						u.dbl_val = -5.12345;
						printf("Original double value: %f\n", u.dbl_val);
						//printf("Binary representation (as long long): 0x%llx\n", u.int_val);

						// Example: flipping the sign bit (most significant bit)
						// 0x8000000000000000ULL has the MSB set to 1
						u.int_val ^= 0x8000000000000000ULL;

						printf("Double value after bit flip: %f\n", u.dbl_val);

						return 0;
				}"""

		checkResults(code)
	}

	"FP and floating point casting" should "print the correct results" in {
		val code =
			"""
				int main() {
						double x = -5.12345;
						printf("Original double value: %f\n", x);

						// Cast the address of the double to a long long pointer
						long long *ptr = (long long *)&x;

						// Apply bitwise AND operation to the underlying bits
						*ptr &= 0x7FFFFFFFFFFFFFFFLL; // Clears the sign bit

						printf("Double value after bitwise AND: %f\n", x); // Will print the absolute value
						return 0;
				}"""

		checkResults(code)
	}
}

class ExtremeFP extends StandardTest {

	"extreme floating point" should "print the correct results" in {
		val code =
			"""#include <stdio.h>

      int main()
      {
      double inf = 1/0.0;
      double minus_inf = -1/0.0;
      double minus_zero = -1/ inf ;
      double nan = 0.0/0.0;

      printf("positive infinity: %f\n",inf);
      printf("negative infinity: %f\n",minus_inf);
      printf("negative zero: %f\n",minus_zero);
      printf("not a number: %f\n",nan);

      /* some arithmetic */

      printf("+inf + 2.0 = %f\n",inf + 2.0);
      printf("+inf - 10.1 = %f\n",inf - 10.1);
      printf("+inf + -inf = %f\n",inf + minus_inf);
      printf("0.0 * +inf = %f\n",0.0 * inf);
      printf("1.0/-0.0 = %f\n",1.0/minus_zero);
      printf("NaN + 1.0 = %f\n",nan + 1.0);
      printf("NaN + NaN = %f\n",nan + nan);

      /* some comparisons */

      printf("NaN == NaN = %s\n",nan == nan ? "true" : "false");
      printf("0.0 == -0.0 = %s\n",0.0 == minus_zero ? "true" : "false");

      return 0;
      }"""

		checkResults(code)
	}
}

class FloatingPoint extends StandardTest {
	"A simple double initialized global reference" should "print the correct results" in {
		val code =
			"""
      double x = 1.5;
      void main() {
        printf("%.1f\n", x);
      }"""

		checkResults(code)
	}

	"A simple double global reassignment" should "print the correct results" in {
		val code =
			"""
      double x = 1.5;
      void main() {
        x = 3.45;
        printf("%f\n", x);
      }"""

		checkResults(code)
	}

	"A simple uninitialized double global reassignment" should "print the correct results" in {
		val code =
			"""
      double x;
      void main() {
        x = 5.12;
        printf("%f\n", x);
      }"""

		checkResults(code)
	}

	"A simple function returning a double" should "print the correct results" in {
		val code =
			"""
      double test() {
         return 4.34;
      }
      void main() {
        double x = test();
        printf("%f\n", x);
      }"""

		checkResults(code)
	}

	"A simple expression with doubles" should "print the correct results" in {
		val code =
			"""
      void main() {
        printf("%f\n", 1.5 * 1.5);
      }"""

		checkResults(code)
	}

	"floating point comparisons" should "print the correct results" in {
		val code =
			"""
				void main() {
					double x = 1.0;
					float y = 4324.5435345;
					float z = 4324.5435346;
			    int zz = x == x;
					printf("%f %d\n", x == y, zz);
				}"""

		checkResults(code)
	}
}
