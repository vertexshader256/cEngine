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
								if (n) {
										printf("\"%s\" = rep-string \"%.*s\"\n", strs[i], n, strs[i]);
								} else {
										printf("\"%s\" = not a rep-string\n", strs[i]);
								}
						}

						return 0;
				}
						"""

		checkResults(code)
	}

	"Repeat a string" should "print the correct results" in {
		val code =
			"""
				#include <stdio.h>
				#include <stdlib.h>
				#include <string.h>

				char * string_repeat( int n, const char * s ) {
					size_t slen = strlen(s);
					char * dest = malloc(n*slen+1);

					int i; char * p;
					for ( i=0, p = dest; i < n; ++i, p += slen ) {
						memcpy(p, s, slen);
					}
					*p = '\0';
					return dest;
				}

				int main() {
					char * result = string_repeat(5, "ha");
					puts(result);
					free(result);
					return 0;
				}
							"""

		checkResults(code)
	}

	"Set consolidation" should "print the correct results" in {
		val code =
			"""
				#include <stdio.h>

				#define s(x) (1U << ((x) - 'A'))

				typedef unsigned int bitset;

				int consolidate(bitset *x, int len)
				{
					int i, j;
					for (i = len - 2; i >= 0; i--)
						for (j = len - 1; j > i; j--)
							if (x[i] & x[j])
								x[i] |= x[j], x[j] = x[--len];
					return len;
				}

				void show_sets(bitset *x, int len)
				{
					bitset b;
					while(len--) {
						for (b = 'A'; b <= 'Z'; b++)
							if (x[len] & s(b)) printf("%c ", b);
						putchar('\n');
					}
				}

				int main(void)
				{
					bitset x[] = { s('A') | s('B'), s('C') | s('D'), s('B') | s('D'),
							s('F') | s('G') | s('H'), s('H') | s('I') | s('K') };

					int len = sizeof(x) / sizeof(x[0]);

					puts("Before:"); show_sets(x, len);
					puts("\nAfter:"); show_sets(x, consolidate(x, len));
					return 0;
				}
								"""

		checkResults(code)
	}

	"Panagram" should "print the correct results" in {
		val code =
			"""
				#include <stdio.h>

				int is_pangram(const char *s)
				{
					const char *alpha = ""
						"abcdefghjiklmnopqrstuvwxyz"
						"ABCDEFGHIJKLMNOPQRSTUVWXYZ";

					char ch, wasused[26] = {0};
					int total = 0;

					while ((ch = *s++) != '\0') {
						const char *p;
						int idx;

						if ((p = strchr(alpha, ch)) == NULL)
							continue;

						idx = (p - alpha) % 26;

						total += !wasused[idx];
						wasused[idx] = 1;
						if (total == 26)
							return 1;
					}
					return 0;
				}

				int main(void)
				{
					int i;
					const char *tests[] = {
						"The quick brown fox jumps over the lazy dog.",
						"The qu1ck brown fox jumps over the lazy d0g."
					};

					for (i = 0; i < 2; i++)
						printf("\"%s\" is %sa pangram\n",
							tests[i], is_pangram(tests[i])?"":"not ");
					return 0;
				}
									"""

		checkResults(code)
	}

	"Range extraction" should "print the correct results" in {
		val code =
			"""
				#include <stdio.h>
				#include <stdlib.h>

				size_t rprint(char *s, int *x, int len)
				{
				#define sep (a > s ? "," : "") /* use comma except before first output */
				#define ol (s ? 100 : 0)       /* print only if not testing for length */
					int i, j;
					char *a = s;
					for (i = j = 0; i < len; i = ++j) {
						for (; j < len - 1 && x[j + 1] == x[j] + 1; j++);

						if (i + 1 < j)
							a += snprintf(s?a:s, ol, "%s%d-%d", sep, x[i], x[j]);
						else
							while (i <= j)
								a += snprintf(s?a:s, ol, "%s%d", sep, x[i++]);
					}
					return a - s;
				#undef sep
				#undef ol
				}

				int main()
				{
					int x[] = {	0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
							15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
							25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
							37, 38, 39 };

					char *s = malloc(rprint(0, x, sizeof(x) / sizeof(int)) + 1);
					rprint(s, x, sizeof(x) / sizeof(int));
					printf("%s\n", s);

					return 0;
				}
				"""

		checkResults(code)
	}

	"Power set" should "print the correct results" in {
		val code =
			"""
					#include <stdio.h>

					struct node {
						char *s;
						struct node* prev;
					};

					void powerset(char **v, int n, struct node *up)
					{
						struct node me;

						if (!n) {
							putchar('[');
							while (up) {
								printf(" %s", up->s);
								up = up->prev;
							}
							puts(" ]");
						} else {
							me.s = *v;
							me.prev = up;
							powerset(v + 1, n - 1, up);
							powerset(v + 1, n - 1, &me);
						}
					}

					int main(int argc, char **argv)
					{
						powerset(argv + 1, argc - 1, 0);
						return 0;
					}
					"""

		checkResults(code, args = List("1","2","3"))
	}

	"Luhn test" should "print the correct results" in {
		val code =
			"""
				#include <string.h>
				#include <stdio.h>

				int luhn(const char* cc)
				{
					const int m[] = {0,2,4,6,8,1,3,5,7,9}; // mapping for rule 3
					int i, odd = 1, sum = 0;

					for (i = strlen(cc); i--; odd = !odd) {
						int digit = cc[i] - '0';
						sum += odd ? digit : m[digit];
					}

					return sum % 10 == 0;
				}

				int main()
				{
					const char* cc[] = {
						"49927398716",
						"49927398717",
						"1234567812345678",
						"1234567812345670",
						0
					};
					int i;

					for (i = 0; cc[i]; i++)
						printf("%16s\t%s\n", cc[i], luhn(cc[i]) ? "ok" : "not ok");

					return 0;
				}
				"""

		checkResults(code)
	}

	"Zsigmondy numbers" should "print the correct results" in {
		val code =
			"""
				#include <stdbool.h>
				#include <stdint.h>
				#include <stdio.h>

				uint64_t ipow(uint64_t n, uint64_t p) {
						if (n == 1 || p == 0) return 1;
						uint64_t x = ipow(n, p>>1);
						return p&1 ? x*x*n : x*x;
				}

				uint64_t gcd(uint64_t a, uint64_t b) {
						return b ? gcd(b, a%b) : a;
				}

				bool all_coprime(uint64_t a, uint64_t b, uint64_t d, uint64_t n) {
						for (uint64_t m = 1; m < n; m++) {
								uint64_t dm = ipow(a,m)-ipow(b,m);
								if (gcd(dm, d) != 1) return false;
						}

						return true;
				}

				uint64_t zsigmondy(uint64_t n, uint64_t a, uint64_t b) {
						uint64_t dn = ipow(a,n) - ipow(b,n);

						uint64_t maxdiv = 0;
						for (uint64_t d = 1; d*d <= dn; d++) {
								if (dn % d != 0) continue;
								if (all_coprime(a, b, d, n))
										maxdiv = d > maxdiv ? d : maxdiv;

								uint64_t dnd = dn/d;
								if (all_coprime(a, b, dnd, n))
										maxdiv = dnd > maxdiv ? dnd : maxdiv;
						};

						return maxdiv;
				}

				void zsig_row(uint64_t a, uint64_t b) {
						printf("zsigmondy(n, %lu, %lu):\n", a, b);
						for (uint64_t n = 1; n <= 18; n++) {
								printf("%lu ", zsigmondy(n, a, b));
						}
						printf("\n");
				}

				int main(void) {
						uint64_t pairs[][2] = {
								{2, 1}
						};

						for (size_t pair=0; pair<sizeof(pairs)/sizeof(*pairs); pair++) {
								zsig_row(pairs[pair][0], pairs[pair][1]);
						}

						return 0;
				}
					"""

		checkResults(code)
	}

	// this test seems to use a lot of memory
	"Pells equation" should "print the correct results" in {
		val code =
			"""
				#include <math.h>
				#include <stdbool.h>
				#include <stdint.h>
				#include <stdio.h>

				struct Pair {
						uint64_t v1, v2;
				};

				struct Pair makePair(uint64_t a, uint64_t b) {
						struct Pair r;
						r.v1 = a;
						r.v2 = b;
						return r;
				}

				struct Pair solvePell(int n) {
						int x = (int) sqrt(n);

						if (x * x == n) {
								// n is a perfect square - no solution other than 1,0
								return makePair(1, 0);
						} else {
								// there are non-trivial solutions
								int y = x;
								int z = 1;
								int r = 2 * x;
								struct Pair e = makePair(1, 0);
								struct Pair f = makePair(0, 1);
								uint64_t a = 0;
								uint64_t b = 0;

								while (true) {
										y = r * z - y;
										z = (n - y * y) / z;
										r = (x + y) / z;
										e = makePair(e.v2, r * e.v2 + e.v1);
										f = makePair(f.v2, r * f.v2 + f.v1);
										a = e.v2 + x * f.v2;
										b = f.v2;
										if (a * a - n * b * b == 1) {
												break;
										}
								}

								return makePair(a, b);
						}
				}

				void test(int n) {
						struct Pair r = solvePell(n);
						printf("x^2 - %3d * y^2 = 1 for x = %21llu and y = %21llu\n", n, r.v1, r.v2);
				}

				int main() {
						test(61);
						//test(109);
						//test(181);
						//test(277);

						return 0;
				}
						"""

		checkResults(code)
	}

	"Phrase reversal" should "print the correct results" in {
		val code =
			"""
					#include <stdio.h>
					#include <string.h>

					/* The functions used are destructive, so after each call the string needs
					 * to be copied over again. One could easily allocate new strings as
					 * required, but this way allows the caller to manage memory themselves */

					char* reverse_section(char *s, size_t length)
					{
							if (length == 0) return s;

							size_t i; char temp;
							for (i = 0; i < length / 2 + 1; ++i)
									temp = s[i], s[i] = s[length - i], s[length - i] = temp;
							return s;
					}

					char* reverse_words_in_order(char *s, char delim)
					{
							if (!strlen(s)) return s;

							size_t i, j;
							for (i = 0; i < strlen(s) - 1; ++i) {
									for (j = 0; s[i + j] != 0 && s[i + j] != delim; ++j)
											;
									reverse_section(s + i, j - 1);
									s += j;
							}
							return s;
					}

					char* reverse_string(char *s)
					{
							return strlen(s) ? reverse_section(s, strlen(s) - 1) : s;
					}

					char* reverse_order_of_words(char *s, char delim)
					{
							reverse_string(s);
							reverse_words_in_order(s, delim);
							return s;
					}

					int main(void)
					{
							char str[]    = "rosetta code phrase reversal";
							size_t lenstr = sizeof(str) / sizeof(str[0]);
							char scopy[lenstr];
							char delim = ' ';

							/* Original String */
							printf("Original:       \"%s\"\n", str);

							/* Reversed string */
							strncpy(scopy, str, lenstr);
							reverse_string(scopy);
							printf("Reversed:       \"%s\"\n", scopy);

							/* Reversed words in string */
							strncpy(scopy, str, lenstr);
							reverse_words_in_order(scopy, delim);
							printf("Reversed words: \"%s\"\n", scopy);

							/* Reversed order of words in string */
							strncpy(scopy, str, lenstr);
							reverse_order_of_words(scopy, delim);
							printf("Reversed order: \"%s\"\n", scopy);

							return 0;
					}
					"""

		checkResults(code)
	}
}
