package scala.c.engine

class printf extends StandardTest {
	//  "Different basic prints non bootstrapped" should "print the correct results" in {
	//    val code = """
	//      void main() {
	//        printf("Hello World!\n");
	//         printf("%s %s\n", "Hello", "World!");
	//         printf("%d\n", 1);
	//         printf("%s\n", "Hello World!");
	//         printf("%.2f\n", 1.47453);
	//         printf("%.1f\n", 1.47453);
	//         printf("%f\n", 1.5f);
	//         printf("\\n");
	//         printf("aa\r\nbb\r\ncc\r\n\r\n");
	//         printf("b.\\s*\\n");
	//      }"""
	//
	//    checkResults(code, false)
	//  }

	"printing a long long as a %d" should "print the correct results" in {
		val code =
			"""
				int main() {
					unsigned long long x = 0xFFFFFFFF;
					printf("after: %d\n", x);
					return 0;
				}"""

		checkResults(code)
	}

	"printing a string with inline pointer arithmetic" should "print the correct results" in {
		val code =
			"""
      void main() {
        char str[] = "Hello!\n";
        printf("%s", str);
        printf("%s", str + 1);
      }"""

		checkResults(code)
	}
}

class printfCustom extends StandardTest {
	"hello world with custom print function" should "print the correct results" in {
		val code =
			"""
      void main() {
        int x = 101;
        printf("Hello World!\n");
        printf("%d\n", 10);
        printf("%d\n", -10);
        printf("%d %d\n", 10, 15);
        printf("%c\n", 'a');
        printf("%s\n", "Just a test!");
        printf("%s\n", 0);
        printf("%.2f\n", 1.47453);
        printf("%.1f\n", 1.47453);
        printf("%f\n", 0.0005);
        printf("%f\n", -345.2783);
        printf("%s %c %d\n", "Just a test!", 'K', 378437);
      }"""

		checkResults(code, true)
	}
}