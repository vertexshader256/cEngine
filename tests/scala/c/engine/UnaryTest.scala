package scala.c.engine

class UnaryTest extends StandardTest {
  
  "A simple binary NOT test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        int y = 0;
        printf("%d %d\n", !x, !y);
      }"""

    checkResults(code)
  }
  
  "A simple postfix decrement test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        float y = 4.5;
        double z = 5.4;
        short u = 2;
        x--;
        y--;
        z--;
        u--;
        printf("%d %f %f %d\n", x, y, z, u);
      }"""

    checkResults(code)
  }
  
  "A simple postfix increment test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        float y = 4.5;
        double z = 5.4;
        short u = 2;
        x++;
        y++;
        z++;
        u++;
        printf("%d %f %f %d\n", x, y, z, u);
      }"""

    checkResults(code)
  }
  
  "A simple prefix increment test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        float y = 4.5;
        double z = 5.4;
        short u = 2;
        ++x;
        ++y;
        ++z;
        ++u;
        printf("%d %f %f %d\n", x, y, z, u);
      }"""

    checkResults(code)
  }
  
  "A simple prefix decrement test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        float y = 4.5;
        double z = 5.4;
        short u = 2;
        --x;
        --y;
        --z;
        --u;
        printf("%d %f %f %d\n", x, y, z, u);
      }"""

    checkResults(code)
  }
  
  "A simple prefix init test" should "print the correct results" in {
    val code = """
      void main() {
        int a = 0;
        int b = ++a;
        int c = 5;
        int d = --c;
        printf("%d %d %d %d\n", a, b, c, d);
      }"""

    checkResults(code)
  }
  
  "A simple postfix init test" should "print the correct results" in {
    val code = """
      void main() {
        int a = 0;
        int b = a++;
        int c = 5;
        int d = c--;
        printf("%d %d %d %d\n", a, b, c, d);
      }"""

    checkResults(code)
  }
  
  "A a negative number and variable" should "print the correct results" in {
    val code = """
      
      int func() {
        return 5;
      }
      
      double func2() {
        return 5.5;
      }
      
      void main() {
        int a = 5;
        double x = 4.2;
        printf("%d %d %d %f %f\n", -6, -a, -func(), -func2(), -x);
      }"""

    checkResults(code)
  }
}
