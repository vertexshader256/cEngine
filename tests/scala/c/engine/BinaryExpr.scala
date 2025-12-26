package scala.c.engine

class StagingArea extends StandardTest {

}

class RobustBitwiseTests extends StandardTest {
  def bitwise(operator: Char) = {
    """
       void main() {
        int a = 5;
        char b = 64;
        long c = 3454;
        short d = 123;
        long long e = 5476578934653;
        """ + {
      val types = List('a', 'b', 'c', 'd', 'e')
      val perms = types.combinations(2).flatMap{x => x.permutations}.toList ++ types.map(x => List(x, x))

      val result = perms.map { perm =>
        """printf("%d\n", """ + "(int)(" + perm(0) + operator + perm(1) + "));"
      }

      val longCases =
        """
            printf("%lld\n", 223372036854775807L """ + operator + """ 2);
            printf("%lld\n", 223372036854775807L """ + operator + """ 72036854775807L);
            printf("%lld\n", 5435 """ + operator + """ 36854775807L);
          """.stripMargin

      result.reduce(_ + "\n" + _) + longCases
    } + "}"
  }

  "bitwise OR robustness test" should "print the correct results" in {
    val code = bitwise('|')
    checkResults(code)
  }

  "bitwise AND robustness test" should "print the correct results" in {
    val code = bitwise('&')
    checkResults(code)
  }

  "bitwise XOR robustness test" should "print the correct results" in {
    val code = bitwise('^')
    checkResults(code)
  }

  "modulus robustness test" should "print the correct results" in {
    val code = bitwise('%')
    checkResults(code)
  }
}

class RobustModulusTests extends StandardTest {
  def modulus(operator: Char) = {
    """
       void main() {
        int a = 5;
        char b = 64;
        long c = 3454;
        short d = 123;
        long long e = 5476578934653;
        """ + {
      val types = List('a', 'b', 'c', 'd', 'e')
      val perms = types.combinations(2).flatMap{x => x.permutations}.toList ++ types.map(x => List(x, x))

      val result = perms.map { perm =>
        """printf("%d\n", """ + "(int)(" + perm(0) + operator + perm(1) + "));"
      }

      val longCases =
        """
            printf("%lld\n", 223372036854775807L """ + operator + """ 2);
            printf("%lld\n", 223372036854775807L """ + operator + """ 72036854775807L);
            printf("%lld\n", 5435 """ + operator + """ 36854775807L);
          """.stripMargin

      result.reduce(_ + "\n" + _) + longCases
    } + "}"
  }

  "modulus robustness test" should "print the correct results" in {
    val code = modulus('%')
    checkResults(code)
  }
}

class RobustDivisionTests extends StandardTest {
  def robustDivide(operator: Char) = {

    """
       void main() {
        int a = 5565643;
        char b = 64;
        float c = 17342.5f;
        double d = 756788.3;
        short f = 123;
        """ + {
      val types = List('a', 'b', 'c', 'd', 'f')
      val perms = types.combinations(2).flatMap{x => x.permutations}.toList ++ types.map(x => List(x, x))

      val result = perms.map{perm =>
        if (perm(0) == 'c' || perm(1) == 'c' || perm(0) == 'd' || perm(1) == 'd') {
          """printf("%.4f\n", """ + perm(0) + operator + perm(1) + ");"
        } else {
          """printf("%d\n", """ + perm(0) + operator + perm(1) + ");"
        }
      }

      val longCases =
        """
          printf("%lld\n", 223372036854775807L / 2);
          printf("%lld\n", 223372036854775807L / 72036854775807L);
          //printf("%.5f\n", 223372036854775807L / 4543.24234);  TODO: fix this one
          printf("%.5f\n", 423784682734.3623543 / 223372036854775807L);
          printf("%.5f\n", 3242.33443f / 223372036854775807L);
          printf("%.5f\n", 223372036854775807L / 3242.33443f);
          printf("%lld\n", 5435 / 223372036854775807L);
          """.stripMargin

      val res = result.reduce(_ + "\n" + _) + longCases

      println(res)
      res
    } + "}"
  }

  "Division robustness test" should "print the correct results" in {
    val code = robustDivide('/')
    checkResults(code)
  }
}

class RobustTests extends StandardTest {
  
  def robust(operator: Char) = {
         """
       void main() {
        int a = 5;
        char b = 64;
        float c = 1.5f;
        double d = 756.3;
        long e = 3454;
        short f = 123;
        //long long g = 5476578934653;
        """ + {
          val types = List('a', 'b', 'c', 'd', 'e', 'f')
          val perms = types.combinations(2).flatMap{x => x.permutations}.toList ++ types.map(x => List(x, x))

          val result = perms.map{perm =>
            if (perm(0) == 'g' || perm(1) == 'g') {}
            if (operator != '>' && operator != '<' && (perm(0) == 'c' || perm(1) == 'c' || perm(0) == 'd' || perm(1) == 'd')) {
              """printf("%.4f\n", """ + perm(0) + operator + perm(1) + ");"
            } else {
              """printf("%d\n", """ + "(int)(" + perm(0) + operator + perm(1) + "));"
            }
          }

           val longCases =
             """
            printf("%lld\n", 223372036854775807L """ + operator + """ 2);
            printf("%lld\n", 223372036854775807L """ + operator + """ 72036854775807L);
            printf("%.5f\n", 6854775807L """ + operator + """ 4543.24234);
            printf("%.5f\n", 423682734.3623543 """ + operator + """ 54775807L);
            printf("%.5f\n", 3242.33443f """ + operator + """ 854775807L);
            printf("%.5f\n", 36854775807L """ + operator + """ 3242.33443f);
            printf("%lld\n", 5435 """ + operator + """ 36854775807L);
          """.stripMargin

          result.reduce(_ + "\n" + _) + longCases
        } + "}"
    }

  def binary(operator: String) = {
    """
       void main() {
        int a = 5;
        char b = 64;
        float c = 1.5f;
        double d = 756.3;
        long e = 3454;
        short f = 123;
        long long g = 5476578934653;
        """ + {
      val types = List('a', 'b', 'c', 'd', 'e', 'f', 'g')
      val perms = types.combinations(2).flatMap{x => x.permutations}.toList ++ types.map(x => List(x, x))

      val result = perms.map{perm =>
        """printf("%d\n", """ + perm(0) + operator + perm(1) + ");"
      }
      result.reduce(_ + "\n" + _)
    } + "}"
  }



  "Addition robustness test" should "print the correct results" in {
    val code = robust('+')
    checkResults(code)
  }
  
  "Subtraction robustness test" should "print the correct results" in {
    val code = robust('-')
    checkResults(code)
  }
  
  "Multiplication robustness test" should "print the correct results" in {
    val code = robust('*')
    checkResults(code)
  }

  "Greater than robustness test" should "print the correct results" in {
    val code = binary(">")
    checkResults(code)
  }

  "Less than robustness test" should "print the correct results" in {
    val code = binary("<")
    checkResults(code)
  }

  "Greater than or equal robustness test" should "print the correct results" in {
    val code = binary(">=")
    checkResults(code)
  }

  "Less than or equal robustness test" should "print the correct results" in {
    val code = binary("<=")
    checkResults(code)
  }
}

class BinaryExpr extends StandardTest {

  "A simple left shift test" should "print the correct results" in {
    val code = """
      void main() {
        unsigned long hash = 193471921;
        hash <<= 1;
        printf("%d\n", (hash << 1));
      }
               """
    checkResults(code)
  }

  "A simple right shift test" should "print the correct results" in {
    val code = """
      void main() {
        unsigned long hash = 193471921;
        hash >>= 2;
        printf("%d\n", (hash >> 5));
      }
               """
    checkResults(code)
  }

  "Order of operations test 3" should "print the correct results" in {
    val code = """
      void main() {
        if ((1 + 2) * (5 - 2) == 9) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
      }"""

    checkResults(code)
  }

  "Two expressions ANDed" should "print the correct results" in {
    val code = """
      void main() {
        if (1 == 1 && 2 == 3) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
        
        if (5 < 10 && 3 == 3) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
      }"""

    checkResults(code)
  }
  
  "AND expression with a char" should "print the correct results" in {
    val code = """
      void main() {
        char x[3] = {1,2,3};
        if (1 == 1 && x[2]) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
        
        if (x[1] && 3 == 3) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
      }"""

    checkResults(code)
  }

  "Two expressions ORed" should "print the correct results" in {
    val code = """
      void main() {
      
        // first expr is true
        if (1 == 1 || 2 == 3) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
        
        // second expr is true
        if (1 == 0 || 7 > 3) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
      }"""

    checkResults(code)
  }

  "Function calls as expressions" should "print the correct results" in {
    val code = """
      int test() {
        return 2;
      }
      
      int test2() {
        return 3;
      }

      void main() {
        if (1 == 1 || 2 == test()) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
        
        if (2 == test() || 1 == 0) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
        
        if (test() == test2() - 1) {
          printf("path1\n");
        } else {
          printf("path2\n");
        }
      }"""

    checkResults(code)
  }

  "A simple increment test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        double y = 5;
        x += 1;
        y += 1.5;
        printf("%d %f\n", x, y);
      }"""

    checkResults(code)
  }
  
  "A more advanced increment test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 15;
        char v[5] = {43,45,21,53,1};
        x += v[2];
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "A more advanced decrement test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 15;
        char v[5] = {43,45,21,53,1};
        x -= v[2];
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
  "Comparing two pointers" should "print the correct results" in {
    val code = """
      void main() {
        int *x = 0;
        int *y = 0;
        printf("%d\n", x == y); // evals to 1 but looks like undefined behavior
      }"""

    checkResults(code)
  }
  
  "short-circuiting" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        int y = 0;
        if (5 < 10 || ++x > 0) {
          printf("%d\n", x);
        }
        
        if (5 > 10 && ++y > 0) {
          printf("%d\n", y);
        } else {
          printf("%d\n", y);
        }
      }"""

    checkResults(code)
  }
  
  "A simple decrement test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        double y = 5;
        x -= 2;
        y -= 2.0;
        printf("%d %f\n", x, y);
      }"""

    checkResults(code)
  }
  
  "A more complex decrement test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 5;
        x -= (x * 4) / 2 + (2 + x) * 2;
        printf("%d\n", x);
      }"""

    checkResults(code)
  }
  
   
  "A modulus test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 20;
        printf("%d %d %d\n", 10 % 2, x % 10, 10 % x);
      }"""

    checkResults(code)
  }
  
  "A binary OR test" should "print the correct results" in {
    val code = """
      void main() {
        int x = 2147483647;
        printf("%d %d\n", 10 | 2, 1 | x);
      }"""

    checkResults(code)
  }
}
