package scala.c.engine

class VariableTest extends StandardTest {
	"make sure variables can shadow" should "print the correct results" in {
		val code =
			"""
				int main(int argc, char **argv){
					int i;

					for(i = 0; i < 20; i++){
							int i = 10;
					}

					printf("done\n");
				}

				"""

		checkResults(code)
	}
}
