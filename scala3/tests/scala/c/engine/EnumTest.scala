package scala.c.engine


class EnumTypedefArithmetic extends StandardTest2("Enum Typedef Arithmetic",
	"""
      typedef enum { ENCRYPT, DECRYPT } cmode;

      int main()
      {
        cmode x = ENCRYPT;
        x++;
        printf("%d\n", x);
      }
  """
)

class EnumTest extends StandardTest {

	"A simple enum" should "print the correct results" in {
		val code =
			"""

      enum { TEST = 1 };

      void main() {
        printf("%d\n", TEST);
      }"""

		checkResults(code)
	}

	"thorough enum tests" should "print the correct results" in {
		val code =
			"""

				typedef enum { TEST = 1, DUO = 3 } testType;

				testType testMe(testType x) {
					int array[3] = {x, x, DUO};
					printf("%d\n", !x);
					if (x > 0) {
						printf("5\n");
					}
					for(int i = x; i < DUO; i++) {
						printf("%d\n", i);
					}
					printf("%d\n", sizeof(x));
					printf("%d\n", array[x]);
			    printf("%d\n", x * 3);
			    return x;
				}

				void main() {
					testType array[3] = {TEST, TEST, DUO};
			    testType array2[3][3] = {{TEST, TEST, DUO},{TEST, TEST, DUO},{TEST, TEST, DUO}};
					testType (*ptr_to_array)[3] = array2;
			    printf("%d\n", ptr_to_array[0][2]);
			    printf("%d\n", ptr_to_array[0][1]);
					printf("%d\n", !TEST);
					testType *ptr = &array[2];
					if (TEST > 0) {
						printf("5\n");
					}
					for(int i = TEST; i < DUO; i++) {
						printf("%d\n", i);
					}
			    long long y = 3;
			    short z = 2;
			    char zz = 1;
			    testType x = (testType)y;
			    x = (testType)z;
			    x = (testType)zz;
			    testType *ptr2 = (testType*)ptr;
					testMe(*ptr);
					printf("%d\n", array[TEST]);
			    printf("%d\n", DUO * 3);
				}"""

		checkResults(code)
	}

	"An advanced enum" should "print the correct results" in {
		val code =
			"""

      enum {
         TE_VARIABLE,

         TE_FUNCTION0 = 8, TE_FUNCTION1, TE_FUNCTION2, TE_FUNCTION3,
         TE_FUNCTION4, TE_FUNCTION5, TE_FUNCTION6, TE_FUNCTION7,

         TE_CLOSURE0 = 16, TE_CLOSURE1, TE_CLOSURE2, TE_CLOSURE3,
         TE_CLOSURE4, TE_CLOSURE5, TE_CLOSURE6, TE_CLOSURE7,

         TE_FLAG_PURE = 32
      };

      void main() {
        printf("%d\n", TE_VARIABLE);
        printf("%d\n", TE_FUNCTION0);
        printf("%d\n", TE_FUNCTION5);
        printf("%d\n", TE_CLOSURE1);
        printf("%d\n", TE_CLOSURE0);
        printf("%d\n", TE_CLOSURE6);
      }"""

		checkResults(code)
	}
}