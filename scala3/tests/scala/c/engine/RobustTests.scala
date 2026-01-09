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

	"Binary numbers numbers" should "print the correct results" in {
		val code =
			"""
				#include <stdio.h>
				#include <stdlib.h>
				#include <string.h>

				typedef struct str_t {
					size_t len, alloc;
					unsigned char *s;
				} bstr_t, *bstr;

				#define str_len(s) ((s)->len)
				bstr str_new(size_t len)
				{
					bstr s = malloc(sizeof(bstr_t));
					if (len < 8) len = 8;
					s->alloc = len;
					s->s = malloc(len);
					s->len = 0;
					return s;
				}

				void str_extend(bstr s)
				{
					size_t ns = s->alloc * 2;
					if (ns - s->alloc > 1024) ns = s->alloc + 1024;
					s->s = realloc(s->s, ns);
					s->alloc = ns;
				}

				void str_del(bstr s)
				{
					free(s->s), free(s);
				}

				int str_cmp(bstr l, bstr r)
				{
					int res, len = l->len;
					if (len > r->len) len = r->len;

					if ((res = memcmp(l->s, r->s, len))) return res;
					return l->len > r->len ? 1 : -1;
				}

				bstr str_dup(bstr src)
				{
					bstr x = str_new(src->len);
					memcpy(x->s, src->s, src->len);
					x->len = src->len;
					return x;
				}

				bstr str_from_chars(const char *t)
				{
					if (!t) return str_new(0);
					size_t l = strlen(t);
					bstr x = str_new(l + 1);
					x->len = l;
					memcpy(x->s, t, l);
					return x;
				}

				void str_append(bstr s, unsigned char b)
				{
					if (s->len >= s->alloc) str_extend(s);
					s->s[s->len++] = b;
				}

				bstr str_substr(bstr s, int from, int to)
				{
					if (!to) to = s->len;
					if (from < 0) from += s->len;
					if (from < 0 || from >= s->len)
						return 0;
					if (to < from) to = from + 1;
					bstr x = str_new(to - from);
					x->len = to - from;
					memcpy(x->s, s->s + from, x->len);
					return x;
				}

				bstr str_cat(bstr s, bstr s2)
				{
					while (s->alloc < s->len + s2->len) str_extend(s);
					memcpy(s->s + s->len, s2->s, s2->len);
					s->len += s2->len;
					return s;
				}

				void str_swap(bstr a, bstr b)
				{
					size_t tz;
					unsigned char *ts;
					tz = a->alloc; a->alloc = b->alloc; b->alloc = tz;
					tz = a->len; a->len = b->len; b->len = tz;
					ts = a->s; a->s = b->s; b->s = ts;
				}

				bstr str_subst(bstr tgt, bstr pat, bstr repl)
				{
					bstr tmp = str_new(0);
					int i;
					for (i = 0; i + pat->len <= tgt->len;) {
						if (memcmp(tgt->s + i, pat->s, pat->len)) {
							str_append(tmp, tgt->s[i]);
							i++;
						} else {
							str_cat(tmp, repl);
							i += pat->len;
							if (!pat->len) str_append(tmp, tgt->s[i++]);
						}
					}
					while (i < tgt->len) str_append(tmp, tgt->s[i++]);
					str_swap(tmp, tgt);
					str_del(tmp);
					return tgt;
				}

				void str_set(bstr dest, bstr src)
				{
					while (dest->len < src->len) str_extend(dest);
					memcpy(dest->s, src->s, src->len);
					dest->len = src->len;
				}

				int main()
				{
					bstr s = str_from_chars("aaaaHaaaaaFaaaaHa");
					bstr s2 = str_from_chars("___.");
					bstr s3 = str_from_chars("");

					str_subst(s, s3, s2);
					printf("%.*s\n", s->len, s->s);

					str_del(s);
					str_del(s2);
					str_del(s3);

					return 0;
				}


						"""

		checkResults(code)
	}
}
