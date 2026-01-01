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
