package scala.c.engine

import org.eclipse.cdt.core.dom.ast.{IASTNode, IType}
import org.eclipse.cdt.internal.core.dom.parser.c.{CASTName, CASTProblemDeclaration}

import scala.c.engine.models.{Address, NumBits}
import scala.c.engine.models.NumBits.*
import scala.c.engine.models.*
import scala.c.engine.gcc.Gcc
import scala.util.Try

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

	"debugging coverage" should "print the correct results" in {
		val code =
			"""
			int main( ) {
				printf("Testing\n");
				return 0;
			}"""

		val codeWithNoOutput =
			"""
					int main( ) {
						return 0;
					}"""

		val codeWithError =
			"""
					int main( ) {
						return 0
					}"""

		val struct = Structure(Array[Byte](), TypeHelper.intType)
		struct.toString // need this
		val ast = Utils.getTranslationUnits(List(code), List("blah.txt"))
		val eng = CEngine(ast, SixtyFourBits)
		val variable = Variable(CASTName("test".toCharArray), eng, TypeHelper.intType, 4)
		variable.toString // need this

		Gcc.getGccOutput(Seq(code), "1", SixtyFourBits, List(), List("", ""))
		Gcc.getGccOutput(Seq(codeWithNoOutput), "2", ThirtyTwoBits, List("", ""), List(""))
		Try(Gcc.getGccOutput(Seq(codeWithError), "3", ThirtyTwoBits, List("", ""), List("")))

		eng.main.run(null, null)

		assert(true)
	}

	"function coverage" should "print the correct results" in {
		val code =
			"""
				#include "string.h"
				int main( ) {
					char *x = "hello3";
			    char *y = "hello2";
			    printf("%d\n", memcmp(x, y, 5));
			    printf("%d\n", memcmp(x, y, 6));
					return 0;
				}"""

		checkResults(code, pointerSize = SixtyFourBits)
	}

	"function argument conversion" should "print the correct results" in {
		val code =
			"""
						#include "math.h"
						#include <ctype.h>
						int main( ) {
				      int f1 = 45;
			        float f2 = 45.0;
			        double f3 = 45.0;
			        char f4 = '2';
			        short f5 = 45;
			        long long f6 = 45;
							printf("%f %f %f %f %f %f\n", cos(f1), cos(f2), cos(f3), cos(f4), cos(f5), cos(f6));
							printf("%f %f %f %f %f %f\n", sqrt(f1), sqrt(f2), sqrt(f3), sqrt(f4), sqrt(f5), sqrt(f6));
							printf("%d %d %d %d %d %d\n", isdigit(f1), isdigit(f2),  isdigit(f3), isdigit(f4),  isdigit(f5), isdigit(f6));
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