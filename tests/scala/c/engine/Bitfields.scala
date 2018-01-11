package scala.c.engine

class BitfieldStagingArea extends StandardTest {
  "A struct with bitfields" should "print the correct results" in {
      val code = """
     struct {
        unsigned int x : 1;
        unsigned int y : 1;
     } status2;

     int main( ) {
        printf( "Memory size occupied by status2 : %d\n", sizeof(status2));
        return 0;
     }"""

    checkResults(code)
  }
}