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

	"testing strcat" should "print the correct results" in {
		val code =
			"""
			#include <string.h>
			void main() {
				char x[20];
			  x[0] = 't';
			  x[1] = 'e';
			  x[2] = 's';
			  x[3] = 't';
			  x[4] = '\0';
				char *y = "append";
			  char *dst = 0;
			  printf("%s\n", x);
			  printf("%s\n", y);
				strcat(x,y);
				printf("%s\n", x);
			}"""

		checkResults(code)
	}

	"sanity test for strtok" should "print the correct results" in {
		val code =
			"""
					#include <string.h>
					void main() {
						char str[] = "a string^&*of, tokens";
						const char delim[] = "*&^"; // Delimiters are space and comma
						char *token;

						// Get the first token
						token = strtok(str, delim);
						printf("Token: %s\n", token);
						token = strtok(NULL, delim); // Use NULL for subsequent calls
			      printf("Token: %s\n", token);
					}"""

		checkResults(code)
	}

	"testing strtok with repeated calls" should "print the correct results" in {
		val code =
			"""
				#include <string.h>
				void main() {
					char str[] = "a string, of, tokens";
					const char delim[] = " ,"; // Delimiters are space and comma
					char *token;

					// Get the first token
					token = strtok(str, delim);

					// Continue getting tokens until there are no more
					while (token != NULL) {
							printf("Token: %s\n", token);
							token = strtok(NULL, delim); // Use NULL for subsequent calls
					}
				}"""

		checkResults(code)
	}
}
