package scala.c.engine

import java.nio.file.Paths

import scala.io.Source

class cjpegTest extends StandardTest {
  "cjpeg test" should "print the correct results" in {

    val list = Paths.get("tests", "scala", "c", "engine", "cjpeg", "cio.c")
    val list2 = Paths.get("tests", "scala", "c", "engine", "cjpeg", "cjpeg.c")
    val list3 = Paths.get("tests", "scala", "c", "engine", "cjpeg", "cmarker.c")
    val list4 = Paths.get("tests", "scala", "c", "engine", "cjpeg", "fdctflt.c")
    val list5 = Paths.get("tests", "scala", "c", "engine", "cjpeg", "rdbmp.c")

    val listText = Source.fromFile(list.toFile, "utf-8").mkString
    val listText2 = Source.fromFile(list2.toFile, "utf-8").mkString
    val listText3 = Source.fromFile(list3.toFile, "utf-8").mkString
    val listText4 = Source.fromFile(list4.toFile, "utf-8").mkString
    val listText5 = Source.fromFile(list5.toFile, "utf-8").mkString

    val allCode = Seq(listText, listText2, listText3, listText4, listText5)

    checkResults2(allCode,  includePaths = List(raw"./tests/scala/c/engine/cjpeg"))
  }
}

//"A struct with more bitfields" should "print the correct results" in {
//  val code = """
//     struct {
//        unsigned int x : 4;
//        unsigned int y : 4;
//        unsigned int x2 : 4;
//        unsigned int y2 : 4;
//        unsigned int x3 : 4;
//        unsigned int y3 : 4;
//        unsigned int x4 : 4;
//        unsigned int y4 : 4;
//        unsigned int z : 1;
//     } status2;
//
//     int main( ) {
//        printf( "Memory size occupied by status2 : %d\n", sizeof(status2));
//        return 0;
//     }"""
//
//  checkResults(code)
//}

//class LimitsTest extends StandardTest2("A limits.h test",
//
//  // https://www.tutorialspoint.com/c_standard_library/limits_h.htm
//  """
//      #include <limits.h>
//
//      int main() {
//
//         printf("The number of bits in a byte %d\n", CHAR_BIT);
//
//         printf("The minimum value of SIGNED CHAR = %d\n", SCHAR_MIN);
//         printf("The maximum value of SIGNED CHAR = %d\n", SCHAR_MAX);
//         printf("The maximum value of UNSIGNED CHAR = %d\n", UCHAR_MAX);
//
//         printf("The minimum value of SHORT INT = %d\n", SHRT_MIN);
//         printf("The maximum value of SHORT INT = %d\n", SHRT_MAX);
//
//         printf("The minimum value of INT = %d\n", INT_MIN);
//         printf("The maximum value of INT = %d\n", INT_MAX);
//
//         printf("The minimum value of CHAR = %d\n", CHAR_MIN);
//         printf("The maximum value of CHAR = %d\n", CHAR_MAX);
//
//         printf("The minimum value of LONG = %ld\n", LONG_MIN);
//         printf("The maximum value of LONG = %ld\n", LONG_MAX);
//
//         return(0);
//      }
//      """
//)

//class UnsignedFromHex extends StandardTest2("unsigned int prints from hex",
//  """
//
//      const unsigned int prime = 0x01000193; //   16777619
//      const unsigned int seed  = 0x811C9DC5; // 2166136261
//
//      void main()
//      {
//        printf("%d %d\n", prime, seed);
//        return 0;
//      }
//      """
//)