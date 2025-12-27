package scala.c.engine

class Recursion extends StandardTest {
	"Recursion test 1" should "print the correct results" in {
		val code =
			"""

      int sums(int x) {
         if (x > 0) {
            return x + sums(x - 1);
         } else {
            return 0;
         }
      }

      void main() {
        printf("%d %d %d %d %d\n", sums(5), sums(4), sums(3), sums(2), sums(1));
      }"""

		checkResults(code)
	}
}
