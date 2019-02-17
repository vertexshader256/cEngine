package scala.c.engine

import java.nio.file.Paths

import better.files.File

class JpegTest extends StandardTest {
  "jpeg test" should "print the correct results" in {
    val code = """
      void main() {
        printf("nation emergency\n");
      }"""

    val slre = Paths.get("tests", "scala", "c", "engine", "jpeg", "jpeg_encoder.c")

    val allCode =  Seq(File(slre).contentAsString)

    checkResults2(allCode, args = List("test.ppm", "10"))
  }
}
