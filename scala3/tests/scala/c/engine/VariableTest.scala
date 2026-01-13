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

	"multiple levels of shadows" should "print the correct results" in {
		val code =
			"""
				int main(int argc, char **argv){
					int i = 10;
					printf("%d\n", i);
					{
						printf("%d\n", i);
						int i = 20;
						printf("%d\n", i);
						i = 30;
						printf("%d\n", i);
						{
							printf("%d\n", i);
							int i = 40;
							printf("%d\n", i);
							i = 50;
			        if (i > 0) {
								printf("%d\n", i);
								int i = 100;
								printf("%d\n", i);
							} else {
								printf("%d\n", i);
								int i = 110;
								printf("%d\n", i);
							}
							printf("%d\n", i);
						}
						printf("%d\n", i);
						i = 60;
						printf("%d\n", i);
					}
					printf("%d\n", i);
					i = 70;
					printf("%d\n", i);
				}

				"""

		checkResults(code)
	}
}
