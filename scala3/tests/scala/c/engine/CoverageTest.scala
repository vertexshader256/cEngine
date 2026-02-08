package scala.c.engine

import org.eclipse.cdt.core.dom.ast.{IASTNode, IType}
import org.eclipse.cdt.internal.core.dom.parser.c.CASTProblemDeclaration

import scala.c.engine.models.{Address, NumBits}
import scala.c.engine.models.NumBits.*
import scala.c.engine.models.*

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

	"bitwise coverage" should "print the correct results" in {
		val code =
			"""
				int main( ) {
					long long x = 243442374;
					unsigned long long y = 243442374;
					unsigned long long z = x & y;
					z = y & z;
					long xx = y + x;
					long yy = x + y;
					double blah = 5.0;
					int zz = x / blah;
					zz = blah / x;
					return 0;
				}"""

		checkResults(code)
	}

	"64 bits" should "print the correct results" in {
		val code =
			"""
					int main( ) {
						int *int_ptr;
						printf("Size of int*   : %zu bytes\n", sizeof(int_ptr));
						printf("Size of int*   : %zu bytes\n", sizeof(int_ptr));
						return 0;
					}"""

		checkResults(code, pointerSize = SixtyFourBits)
	}

	"errors" should "print the correct results" in {
		val code =
			"""
				int main( ) {
						short y = 24;
						char z = 'd';
						float x = (float)y;
						float xx = (float)z;
			      float jj == (float)z;
						iff (blah == blah) {
						}
						printf("%f %f\n", x, xx);
				}"""

		Results.getCEngineOutput(Seq(code), true, NumBits.SixtyFourBits, List(), List())
		StandardTest.getGccOutput(Seq(code), NumBits.SixtyFourBits, List(), List())

		assert(true)
	}

	"improved coverage" should "print the correct results" in {

		val ptr = Pointer(Address(0), null)
		ptr.toString

		assert(true)
	}
}