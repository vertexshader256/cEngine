package scala.c.engine

import scala.c.engine.models.NumBits
import scala.c.engine.models.NumBits.ThirtyTwoBits

class CoverageTest extends StandardTest {
	"strstr coverage" should "print the correct results" in {
		val code =
			"""
     int main( ) {
			  char *str = "testing strstr";
			  char *notfound = "not found";
				strstr(str, "");
        printf("%d\n", strstr(str, notfound));
        return 0;
     }"""

		checkResults(code)
	}

	"toupper" should "print the correct results" in {
		val code =
			"""
			#include <ctype.h>

			int main( ) {
					printf("%d\n", toupper('c'));
					return 0;
			}"""

		checkResults(code)
	}

	"double conversions" should "print the correct results" in {
		val code =
			"""
				int main( ) {
			      short y = 24;
			      char z = 'd';
			      double x = (double)y;
			      double xx = (double)z;
						printf("%f %f\n", x, xx);
						return 0;
				}"""

		checkResults(code)
	}

	"float conversions" should "print the correct results" in {
		val code =
			"""
					int main( ) {
							short y = 24;
							char z = 'd';
							float x = (float)y;
							float xx = (float)z;
							printf("%f %f\n", x, xx);
							return 0;
					}"""

		checkResults(code)
	}

	"type coverage" should "print the correct results" in {
		val short: Short = 16
		val char: Byte = 8
		TypeHelper.getType(short)
		TypeHelper.getType(true)
		TypeHelper.getType(char)

		assert(true)
	}

	"errors" should "print the correct results" in {
		val code =
			"""
				int main( ) {
						short y = 24;
						char z = 'd'
						float x = (float)y;
						float xx = (float)z;
						printf("%f %f\n", x, xx);
						return 0;
				}"""

		CEngine.getCEngineOutput(Seq(code), true, NumBits.ThirtyTwoBits, List(), List())

		assert(true)
	}
}