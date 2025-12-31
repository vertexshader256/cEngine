package scala.c.engine

class UnionTestStaging extends StandardTest {
	
	"union to view floating point bits" should "print the correct results" in {
		val code =
			"""
				int main() {
						union {
								double dbl_val;
								long long int_val;
						} u;
				
						u.dbl_val = -5.12345;
						printf("Original double value: %f\n", u.dbl_val);
						//printf("Binary representation (as long long): 0x%llx\n", u.int_val);
				
						// Example: flipping the sign bit (most significant bit)
						// 0x8000000000000000ULL has the MSB set to 1
						u.int_val ^= 0x8000000000000000ULL; 
				
						printf("Double value after bit flip: %f\n", u.dbl_val);
				
						return 0;
				}"""

		checkResults(code)
	}
	
	"union struct" should "print the correct results" in {
		val code =
			"""

      union Test {
        int y;
        float x;
      } Test;

      void main() {
        union Test test;
        test.x = 334.283;
        printf("%f %d\n", test.x, test.y);
      }"""

		checkResults(code)
	}

	"union struct typedef" should "print the correct results" in {
		val code =
			"""

      typedef union Test {
        int y;
        float x;
      } Test;

      void main() {
        Test test;
        test.x = -2373749;
        printf("%f %d\n", test.x, test.y);
      }"""

		checkResults(code)
	}

	"union struct pointer" should "print the correct results" in {
		val code =
			"""

      typedef union Test {
        int y;
        float x;
      } Test;

      void main() {
        Test test;
        Test *test2 = &test;
        test.x = -2373749;
        printf("%f %d\n", test2->x, test2->y);
      }"""

		checkResults(code)
	}

	"embedded union struct pointer" should "print the correct results" in {
		val code =
			"""

      typedef struct embedded
      {
        unsigned char type;
        union
        {
          int y;
          float x;
        } info;
      } embedded;

      void main() {
        embedded test;
        test.type = 'b';
        test.info.y = 1162651022;
        printf("%c %d %f\n", test.type, test.info.y, test.info.x);
      }"""

		checkResults(code)
	}


}