package scala.c.engine

import java.nio.file.Paths

import scala.io.Source

class JpegTest extends StandardTest {
  "jpeg test" should "print the correct results" in {
    val code = """
      void main() {
        printf("nation emergency\n");
      }"""

    val slre = Paths.get("tests", "scala", "c", "engine", "jpeg", "jpeg_encoder.c")
    val slreText = Source.fromFile(slre.toFile, "utf-8").mkString

    val allCode =  Seq(slreText)

    checkResults2(allCode, args = List("test.ppm", "10"))
  }
}
