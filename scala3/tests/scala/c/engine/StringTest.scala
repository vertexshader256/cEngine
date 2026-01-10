package scala.c.engine

class StringTest extends StandardTest {
	"a simple string test" should "print the correct results" in {
		val code =
			"""
				int main(int argc, char *argv[])
				{
					char array[] = "This is a string assigned to an array";
					char *ptr = "This is a string assigned to an pointer";
					printf("%s %s\n", array, ptr);
					return 0;
				}"""

		checkResults(code)
	}

	"editing a string that is unsigned chars" should "print the correct results" in {
		val code =
			"""
					void main() {
						unsigned char x[] = "test";
						x[2] = 140;
						printf("%d %d %d\n", x[0], x[1], x[2]);
					}"""

		checkResults(code)
	}
}
