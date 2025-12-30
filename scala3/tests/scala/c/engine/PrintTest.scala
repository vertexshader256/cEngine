package scala.c.engine

class PrintTest extends StandardTest {
	"two sequential calls to print" should "print the correct results" in {
		val code =
			"""
				#include <stdio.h>
				#include <stdlib.h>

				int main(int argc, char*argv[])
				{
						printf("test:  ");
						printf("%d", 5);
						return 0;
				}
			"""

		checkResults(code)
	}
}
