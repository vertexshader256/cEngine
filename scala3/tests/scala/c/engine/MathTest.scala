package scala.c.engine

import java.io.File

class MathTest extends StandardTest {

	"math function test" should "print the correct results" in {

		val code =
			"""
      #include <math.h>

      void main() {
			  printf("%f\n", cos(45.0));
				printf("%f\n", sin(45.0));
			  printf("%f\n", tan(45.0));
			  printf("%f\n", asin(0.5));
			  printf("%f\n", sinh(1.0));
			  printf("%f\n", acos(0.5));
			  printf("%f\n", cosh(1.0));
			  printf("%f\n", atan(0.5));
			  printf("%f\n", tanh(1.0));
			  printf("%f\n", ceil(1.5));
			  printf("%f\n", floor(1.5));
			  printf("%f\n", pow(1.5, 3));
			  printf("%f\n", exp(1.5));
			  printf("%f\n", log(1.5));
			  printf("%f\n", atan2(1, 1));
			  printf("%f\n", log10(2));
      }"""

		checkResults(code)
	}
}
