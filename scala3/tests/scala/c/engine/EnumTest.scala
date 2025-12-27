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