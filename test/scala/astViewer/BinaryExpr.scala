package scala.astViewer

class StagingArea extends StandardTest {
  "A simple left shift test" should "print the correct results" in {
    val code = """
      void main() {
        unsigned long hash = 193471921;
        printf("%d\n", (hash << 5));
      }
      """
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
        """ + {
         def combinationWithRepeat(input: List[Char], size:Int) = {List.fill(size)(input).flatten.combinations(size).toList}
          val perms = combinationWithRepeat(List('a', 'b', 'c', 'd', 'e', 'f'), 2).toList.flatMap{x => List(x, x.reverse)}
          val result = perms.map{perm => 
            if (perm(0) == 'c' || perm(1) == 'c' || perm(0) == 'd' || perm(1) == 'd') {
              """printf("%f\n", """ + perm(0) + operator + perm(1) + ");"
            } else {
              """printf("%d\n", """ + perm(0) + operator + perm(1) + ");"
            }
          }
          result.reduce(_ + "\n" + _)
        } + "}"
    }
  
  def robustDivide(operator: Char) = {
         """
       void main() {
        int a = 5;
        char b = 64;
        float c = 1.5f;
        double d = 756.3;
        long e = 3454;
        short f = 123;
        """ + {
         def combinationWithRepeat(input: List[Char], size:Int) = {List.fill(size)(input).flatten.combinations(size).toList}
          val perms = combinationWithRepeat(List('a', 'b', 'c', 'd', 'e', 'f'), 2).toList.flatMap{x => List(x, x.reverse)}
          val result = perms.map{perm => 
            """printf("%f\n", """ + perm(0) + operator + perm(1) + ");"
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
  
//  "Division robustness test" should "print the correct results" in {
//    val code = robustDivide('/')
//    checkResults(code)
//  }
}

class BinaryExpr extends StandardTest {
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
