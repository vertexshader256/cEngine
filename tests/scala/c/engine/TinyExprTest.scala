package scala.c.engine

import java.nio.file.Paths

import better.files.File

class TinyExprTest extends StandardTest {
  "tinyexpr test" should "print the correct results" in {
    val code = """
      void main() {

        #include "tinyexpr.h"

        test_case errors[] = {
         {"", 1},
         {"1+", 2},
         {"1)", 2},
         {"(1", 2},
         {"1**1", 3},
         {"1*2(+4", 4},
         {"1*2(1+4", 4},
         {"a+5", 1},
         {"A+5", 1},
         {"Aa+5", 1},
         {"1^^5", 3},
         {"1**5", 3},
         {"sin(cos5", 8},
     };


     int main() {
       int i;
       for (i = 0; i < sizeof(errors) / sizeof(test_case); ++i) {
           const char *expr = errors[i].expr;
           const int e = errors[i].answer;

           int err;
           const double r = te_interp(expr, &err);
           ASSERT(r != r);

           te_expr *n = te_compile(expr, 0, 0, &err);
           ASSERT(!n);

           if (err != e) {
               printf("FAILED: %s\n", expr);
           }

           const double k = te_interp(expr, 0);
           ASSERT(k != k);
       }
      }
    }"""

    val tinyExpr = Paths.get("tests", "scala", "c", "engine", "tinyexpr", "tinyexpr.c")
    //val testC = Paths.get("tests", "scala", "c", "engine", "tinyexpr", "test.c")

    val allCode =  Seq(File(tinyExpr).contentAsString)

    checkResults2(allCode, includePaths = List(raw"./tests/scala/c/engine/tinyexpr"))
  }
}
