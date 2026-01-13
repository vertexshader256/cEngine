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

	"Brazilian numbers" should "print the correct results" in {
		val code =
			"""

					int same_digits(int n, int b) {
							int f = n % b;
							n /= b;
							while (n > 0) {
									if (n % b != f) return 0;
									n /= b;
							}
							return 1;
					}

					int is_brazilian(int n) {
							int b;
							if (n < 7) return 0;
							if (!(n % 2) && n >= 8) return 1;
							for (b = 2; b < n - 1; ++b) {
									if (same_digits(n, b)) return 1;
							}
							return 0;
					}

					int is_prime(int n) {
							int d = 5;
							if (n < 2) return 0;
							if (!(n % 2)) return n == 2;
							if (!(n % 3)) return n == 3;
							while (d * d <= n) {
									if (!(n % d)) return 0;
									d += 2;
									if (!(n % d)) return 0;
									d += 4;
							}
							return 1;
					}

					int main() {
							int i, c, n;
							const char *kinds[3] = {" ", " odd ", " prime "};
							for (i = 0; i < 3; ++i) {
									printf("First 15%sBrazilian numbers:\n", kinds[i]);
									c = 0;
									n = 7;
									while (1) {
											if (is_brazilian(n)) {
													printf("%d ", n);
													if (++c == 12) {
															printf("\n\n");
															break;
													}
											}
											switch (i) {
													case 0: n++; break;
													case 1: n += 2; break;
													case 2:
															do {
																	n += 2;
															} while (!is_prime(n));
															break;
											}
									}
							}

							return 0;
					}

					"""

		checkResults(code)
	}

	"Department numbers" should "print the correct results" in {
		val code =
			"""
				#include<stdio.h>

				int main()
				{
					int police,sanitation,fire;

					printf("Police     Sanitation         Fire\n");
					printf("----------------------------------");

					for(police=2;police<=6;police+=2){
						for(sanitation=1;sanitation<=7;sanitation++){
							for(fire=1;fire<=7;fire++){
								if(police!=sanitation && sanitation!=fire && fire!=police && police+fire+sanitation==12){
									printf("\n%d\t\t%d\t\t%d",police,sanitation,fire);
								}
							}
						}
					}

					return 0;
				}

				"""

		checkResults(code)
	}

	"tau function" should "print the correct results" in {
		val code =
			"""
				#include <stdio.h>

				// See https://en.wikipedia.org/wiki/Divisor_function
				unsigned int divisor_count(unsigned int n) {
						unsigned int total = 1;
						// Deal with powers of 2 first
						for (; (n & 1) == 0; n >>= 1) {
								++total;
						}
						// Odd prime factors up to the square root
						for (unsigned int p = 3; p * p <= n; p += 2) {
								unsigned int count = 1;
								for (; n % p == 0; n /= p) {
										++count;
								}
								total *= count;
						}
						// If n > 1 then it's prime
						if (n > 1) {
								total *= 2;
						}
						return total;
				}

				int main() {
						const unsigned int limit = 100;
						unsigned int n;

						printf("Count of divisors for the first %d positive integers:\n", limit);
						for (n = 1; n <= limit; ++n) {
								printf("%3d", divisor_count(n));
								if (n % 20 == 0) {
										printf("\n");
								}
						}

						return 0;
				}
			"""

		checkResults(code)
	}

	"topswaps" should "print the correct results" in {
		val code =
			"""
				#include <stdio.h>
				#include <string.h>

				typedef struct { char v[16]; } deck;
				typedef unsigned int uint;

				uint n, d, best[16];

				void tryswaps(deck *a, uint f, uint s) {
				#	define A a->v
				#	define B b.v
					if (d > best[n]) best[n] = d;
					while (1) {
						if ((A[s] == s || (A[s] == -1 && !(f & 1U << s)))
							&& (d + best[s] >= best[n] || A[s] == -1))
							break;

						if (d + best[s] <= best[n]) return;
						if (!--s) return;
					}

					d++;
					deck b = *a;
					for (uint i = 1, k = 2; i <= s; k <<= 1, i++) {
						if (A[i] != i && (A[i] != -1 || (f & k)))
							continue;

						for (uint j = B[0] = i; j--;) B[i - j] = A[j];
						tryswaps(&b, f | k, s);
					}
					d--;
				}

				int main(void) {
					deck x;
					memset(&x, -1, sizeof(x));
					x.v[0] = 0;

					for (n = 1; n < 13; n++) {
						tryswaps(&x, 1, n - 1);
						printf("%2d: %d\n", n, best[n]);
					}

					return 0;
				}
						"""

		checkResults(code)
	}

	"Ternary logic" should "print the correct results" in {
		val code =
			"""
				#include <stdio.h>

				typedef enum {
					TRITTRUE,  /* In this enum, equivalent to integer value 0 */
					TRITMAYBE, /* In this enum, equivalent to integer value 1 */
					TRITFALSE  /* In this enum, equivalent to integer value 2 */
				} trit;

				/* We can trivially find the result of the operation by passing
					 the trinary values as indeces into the lookup tables' arrays. */
				trit tritNot[3] = {TRITFALSE , TRITMAYBE, TRITTRUE};
				trit tritAnd[3][3] = { {TRITTRUE, TRITMAYBE, TRITFALSE},
															 {TRITMAYBE, TRITMAYBE, TRITFALSE},
															 {TRITFALSE, TRITFALSE, TRITFALSE} };

				trit tritOr[3][3] = { {TRITTRUE, TRITTRUE, TRITTRUE},
															{TRITTRUE, TRITMAYBE, TRITMAYBE},
															{TRITTRUE, TRITMAYBE, TRITFALSE} };

				trit tritThen[3][3] = { { TRITTRUE, TRITMAYBE, TRITFALSE},
																{ TRITTRUE, TRITMAYBE, TRITMAYBE},
																{ TRITTRUE, TRITTRUE, TRITTRUE } };

				trit tritEquiv[3][3] = { { TRITTRUE, TRITMAYBE, TRITFALSE},
																 { TRITMAYBE, TRITMAYBE, TRITMAYBE},
																 { TRITFALSE, TRITMAYBE, TRITTRUE } };

				/* Everything beyond here is just demonstration */

				const char* tritString[3] = {"T", "?", "F"};

				void demo_binary_op(trit operator[3][3], const char* name)
				{
					trit operand1 = TRITTRUE; /* Declare. Initialize for CYA */
					trit operand2 = TRITTRUE; /* Declare. Initialize for CYA */

					/* Blank line */
					printf("\n");

					/* Demo this operator */
					for( operand1 = TRITTRUE; operand1 <= TRITFALSE; ++operand1 )
					{
						for( operand2 = TRITTRUE; operand2 <= TRITFALSE; ++operand2 )
						{
							printf("%s %s %s: %s\n", tritString[operand1],
																			 name,
																			 tritString[operand2],
																			 tritString[operator[operand1][operand2]]);
						}
					}

				}

				int main()
				{
					trit op1 = TRITTRUE; /* Declare. Initialize for CYA */
					trit op2 = TRITTRUE; /* Declare. Initialize for CYA */

					/* Demo 'not' */
					for( op1 = TRITTRUE; op1 <= TRITFALSE; ++op1 )
					{
						printf("Not %s: %s\n", tritString[op1], tritString[tritNot[op1]]);
					}
					demo_binary_op(tritAnd, "And");
					demo_binary_op(tritOr, "Or");
					demo_binary_op(tritThen, "Then");
					demo_binary_op(tritEquiv, "Equiv");


					return 0;
				}

					"""

		checkResults(code)
	}

	"Deceptive numbers" should "print the correct results" in {
		val code =
			"""
				#include <inttypes.h>
				#include <stdio.h>

				uint32_t modpow(uint32_t b, uint32_t e, uint32_t m)
				{
						uint32_t p;
						for (p = 1; e; e >>= 1) {
								if (e & 1)
										p = (uint64_t)p * b % m;
								b = (uint64_t)b * b % m;
						}
						return p;
				}

				int is_deceptive(uint32_t n)
				{
						uint32_t x;
						if (n & 1 && n % 3 && n % 5 && modpow(10, n - 1, n) == 1) {
								for (x = 7; x * x <= n; x += 6) {
										if (!(n % x && n % (x + 4)))
												return 1;
								}
						}
						return 0;
				}

				int main(void)
				{
						uint32_t n = 49;
						unsigned int c;
						for (c = 0; c != 20; ++n) {
								if (is_deceptive(n)) {
										printf(" %" PRIu32, n);
										++c;
								}
						}
						return 0;
				}

					"""

		checkResults(code)
	}

	"Quibbing strings" should "print the correct results" in {
		val code =
			"""
				#include <stdio.h>
				#include <string.h>
				#include <stdlib.h>
				
				char *quib(const char **strs, size_t size)
				{
				
						size_t len = 3 + ((size > 1) ? (2 * size + 1) : 0);
						size_t i;
				
						for (i = 0; i < size; i++)
								len += strlen(strs[i]);
				
						char *s = malloc(len * sizeof(*s));
						if (!s)
						{
								perror("Can't allocate memory!\n");
								exit(EXIT_FAILURE);
						}
				
						strcpy(s, "{");
						switch (size) {
								case 0:  break;
								case 1:  strcat(s, strs[0]);
												 break;
								default: for (i = 0; i < size - 1; i++)
												 {
														 strcat(s, strs[i]);
														 if (i < size - 2)
																 strcat(s, ", ");
														 else
																 strcat(s, " and ");
												 }
												 strcat(s, strs[i]);
												 break;
						}  
						strcat(s, "}");
						return s;
				}
				
				int main(void)
				{
						const char *test[] = {"ABC", "DEF", "G", "H"};
						char *s;
				
						for (size_t i = 0; i < 5; i++)
						{
								s = quib(test, i);
								printf("%s\n", s);
								free(s);
						}
						return EXIT_SUCCESS;
				}
	
					"""

		checkResults(code)
	}
}
