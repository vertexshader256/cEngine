package scala.c.engine

class RobustTest extends StandardTest {
	"binary search" should "print the correct results" in {
		val code =
			"""

			int binsearch (int *a, int n, int x) {
					int i = 0, j = n - 1;
					while (i <= j) {
							int k = i + ((j - i) / 2);
							if (a[k] == x) {
									return k;
							}
							else if (a[k] < x) {
									i = k + 1;
							}
							else {
									j = k - 1;
							}
					}
					return -1;
			}

			int binsearch_r (int *a, int x, int i, int j) {
					if (j < i) {
							return -1;
					}
					int k = i + ((j - i) / 2);
					if (a[k] == x) {
							return k;
					}
					else if (a[k] < x) {
							return binsearch_r(a, x, k + 1, j);
					}
					else {
							return binsearch_r(a, x, i, k - 1);
					}
			}

			int main () {
					int a[] = {-31, 0, 1, 2, 2, 4, 65, 83, 99, 782};
					int n = sizeof a / sizeof a[0];
					int x = 2;
					int i = binsearch(a, n, x);
					if (i >= 0)
						printf("%d is at index %d.\n", x, i);
					else
						printf("%d is not found.\n", x);
					x = 5;
					i = binsearch_r(a, x, 0, n - 1);
					if (i >= 0)
						printf("%d is at index %d.\n", x, i);
					else
						printf("%d is not found.\n", x);
					return 0;
			}
			"""

		checkResults(code)
	}

	"benfords law" should "print the correct results" in {
		val code =
			"""

				#include <stdio.h>
				#include <stdlib.h>
				#include <math.h>

				float *benford_distribution(void)
				{
						static float prob[9];
						for (int i = 1; i < 10; i++)
								prob[i - 1] = log10f(1 + 1.0 / i);

						return prob;
				}

				float *get_actual_distribution(char *fn)
				{
						FILE *input = fopen(fn, "r");
						if (!input)
						{
								perror("Can't open file");
								exit(EXIT_FAILURE);
						}

						int tally[9] = { 0 };
						char c;
						int total = 0;
						while ((c = getc(input)) != EOF)
						{
								/* get the first nonzero digit on the current line */
								while (c < '1' || c > '9')
										c = getc(input);

								tally[c - '1']++;
								total++;

								/* discard rest of line */
								while ((c = getc(input)) != '\n' && c != EOF)
										;
						}
						fclose(input);

						static float freq[9];
						for (int i = 0; i < 9; i++)
								freq[i] = tally[i] / (float) total;

						return freq;
				}

				int main(int argc, char **argv)
				{
						if (argc != 2)
						{
								printf("Usage: benford <file>\n");
								return EXIT_FAILURE;
						}

						float *actual = get_actual_distribution(argv[1]);
						float *expected = benford_distribution();

						puts("digit\tactual\texpected");
						for (int i = 0; i < 9; i++)
								printf("%d\t%.3f\t%.3f\n", i + 1, actual[i], expected[i]);

						return EXIT_SUCCESS;
				}

				"""

		checkResults(code, args = List("fib1000.txt"))
	}
}
