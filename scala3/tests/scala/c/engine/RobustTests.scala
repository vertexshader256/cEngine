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

	"zeckendorf arithmatic" should "print the correct results" in {
		val code =
			"""
				#include <stdbool.h>
				#include <stdio.h>
				#include <string.h>

				int inv(int a) {
						return a ^ -1;
				}

				struct Zeckendorf {
						int dVal, dLen;
				};

				void a(struct Zeckendorf *self, int n) {
						void b(struct Zeckendorf *, int); // forward declare

						int i = n;
						while (true) {
								if (self->dLen < i) self->dLen = i;
								int j = (self->dVal >> (i * 2)) & 3;
								switch (j) {
								case 0:
								case 1:
										return;
								case 2:
										if (((self->dVal >> ((i + 1) * 2)) & 1) != 1) return;
										self->dVal += 1 << (i * 2 + 1);
										return;
								case 3:
										self->dVal = self->dVal & inv(3 << (i * 2));
										b(self, (i + 1) * 2);
										break;
								default:
										break;
								}
								i++;
						}
				}

				void increment(struct Zeckendorf *self) {
						self->dVal++;
						a(self, 0);
				}

				void b(struct Zeckendorf *self, int pos) {
						//void increment(struct Zeckendorf *); // forward declare

						if (pos == 0) {
								increment(self);
								return;
						}
						if (((self->dVal >> pos) & 1) == 0) {
								self->dVal += 1 << pos;
								a(self, pos / 2);
								if (pos > 1) a(self, pos / 2 - 1);
						} else {
								self->dVal = self->dVal & inv(1 << pos);
								b(self, pos + 1);
								b(self, pos - (pos > 1 ? 2 : 1));
						}
				}

				void c(struct Zeckendorf *self, int pos) {
						if (((self->dVal >> pos) & 1) == 1) {
								self->dVal = self->dVal & inv(1 << pos);
								return;
						}
						c(self, pos + 1);
						if (pos > 0) {
								b(self, pos - 1);
						} else {
								increment(self);
						}
				}

				struct Zeckendorf makeZeckendorf(char *x) {
						struct Zeckendorf z = { 0, 0 };
						int i = strlen(x) - 1;
						int q = 1;

						z.dLen = i / 2;
						while (i >= 0) {
								z.dVal += (x[i] - '0') * q;
								q *= 2;
								i--;
						}

						printf("HERE2: %d\n", z.dLen);
						return z;
				}

				void addAssign(struct Zeckendorf *self, struct Zeckendorf rhs) {
						int gn;

						printf("%d\n", rhs.dLen);
						for (gn = 0; gn < (rhs.dLen + 1) * 2; gn++) {
			          printf("HERE\n");
								if (((rhs.dVal >> gn) & 1) == 1) {
										b(self, gn);
								}
						}
				}

				void subAssign(struct Zeckendorf *self, struct Zeckendorf rhs) {
						int gn;
						for (gn = 0; gn < (rhs.dLen + 1) * 2; gn++) {
								if (((rhs.dVal >> gn) & 1) == 1) {
										c(self, gn);
								}
						}
						while ((((self->dVal >> self->dLen * 2) & 3) == 0) || (self->dLen == 0)) {
								self->dLen--;
						}
				}

				void mulAssign(struct Zeckendorf *self, struct Zeckendorf rhs) {
						struct Zeckendorf na = rhs;
						struct Zeckendorf nb = rhs;
						struct Zeckendorf nr = makeZeckendorf("0");
						struct Zeckendorf nt;
						int i;

						for (i = 0; i < (self->dLen + 1) * 2; i++) {
								if (((self->dVal >> i) & 1) > 0) addAssign(&nr, nb);
								nt = nb;
								addAssign(&nb, na);
								na = nt;
						}

						*self = nr;
				}

				void printZeckendorf(struct Zeckendorf z) {
						static const char *const dig[3] = { "00", "01", "10" };
						static const char *const dig1[3] = { "", "1", "10" };

						printf("%d\n", z.dVal);
						if (z.dVal == 0) {
								printf("0");
								return;
						} else {
								int idx = (z.dVal >> (z.dLen * 2)) & 3;
								printf("%d\n", idx);
								int i;

								printf(dig1[idx]);
								for (i = z.dLen - 1; i >= 0; i--) {
										idx = (z.dVal >> (i * 2)) & 3;
										printf(dig[idx]);
								}
						}
				}

				int main() {
						struct Zeckendorf g;

						printf("Addition:\n");
						g = makeZeckendorf("10");
						addAssign(&g, makeZeckendorf("10"));
						printZeckendorf(g);
//						printf("\n");
//						addAssign(&g, makeZeckendorf("10"));
//						printZeckendorf(g);
//						printf("\n");
//						addAssign(&g, makeZeckendorf("1001"));
//						printZeckendorf(g);
//						printf("\n");
//						addAssign(&g, makeZeckendorf("1000"));
//						printZeckendorf(g);
//						printf("\n");
//						addAssign(&g, makeZeckendorf("10101"));
//						printZeckendorf(g);
//						printf("\n\n");
//
//						printf("Subtraction:\n");
//						g = makeZeckendorf("1000");
//						subAssign(&g, makeZeckendorf("101"));
//						printZeckendorf(g);
//						printf("\n");
//						g = makeZeckendorf("10101010");
//						subAssign(&g, makeZeckendorf("1010101"));
//						printZeckendorf(g);
//						printf("\n\n");
//
//						printf("Multiplication:\n");
//						g = makeZeckendorf("1001");
//						mulAssign(&g, makeZeckendorf("101"));
//						printZeckendorf(g);
//						printf("\n");
//						g = makeZeckendorf("101010");
//						addAssign(&g, makeZeckendorf("101"));
//						printZeckendorf(g);
//						printf("\n");

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

	"Longest substring" should "print the correct results" in {
		val code =
			"""
				#include <stdio.h>
				#include <string.h>

				int repstr(char *str)
				{
						if (!str) return 0;

						size_t sl = strlen(str) / 2;
						while (sl > 0) {
								if (strstr(str, str + sl) == str)
										return sl;
								--sl;
						}

						return 0;
				}

				int main(void)
				{
						char *strs[] = { "1001110011", "1110111011", "0010010010", "1111111111",
								"0100101101", "0100100", "101", "11", "00", "1" };

						size_t strslen = sizeof(strs) / sizeof(strs[0]);
						size_t i;
						for (i = 0; i < strslen; ++i) {
								int n = repstr(strs[i]);
								printf("%d\n", n);
								if (n)
										printf("\"%s\" = rep-string \"%.*s\"\n", strs[i], n, strs[i]);
								else
										printf("\"%s\" = not a rep-string\n", strs[i]);
						}

						return 0;
				}
						"""

		checkResults(code)
	}
}
