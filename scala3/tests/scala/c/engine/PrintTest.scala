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
