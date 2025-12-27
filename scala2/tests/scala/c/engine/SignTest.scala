package scala.c.engine

class SignTest extends StandardTest {
	"unsigned test 1" should "print the correct results" in {
		val code =
			"""
      void main() {
        unsigned int x = 2147483647;
        printf("%d\n", x);
      }"""

		checkResults(code)
	}

	"a char signed corner case" should "print the correct results" in {
		val code =
			"""
      #include <stdio.h>
      #include <stddef.h>

      int my_read_char(const char *buffer, size_t *offs) {
        if (buffer[*offs] != '\0') {
          return buffer[*offs++];  /* here's the trap */
        } else {
          return 255;
        }
      }

      int my_read_char2(const char *buffer, size_t *offs) {
        if (buffer[*offs] != '\0') {
          return (unsigned char) buffer[*offs++];
        } else {
          return 255;
        }
      }

      void main() {
        char blah[10] = {1, 5, 10, 100, 200, 0};
        size_t offset = 1;
        int i = 0;

        for (i = 0; i < 5; i++) {
          printf("%d\n", my_read_char(blah, &offset));
          printf("%d\n", my_read_char2(blah, &offset));
          offset += 1;
        }



      }"""

		checkResults(code)
	}

	"unsigned test involving negatives" should "print the correct results" in {
		val code =
			"""
      void main() {
        unsigned int x = -10;
        unsigned int y = 10 + x;
        printf("%d\n", y);
      }"""

		checkResults(code)
	}
}
