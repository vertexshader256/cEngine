package scala.c.engine

import java.nio.file.Paths

import scala.c.engine.Interpreter._
import scala.io.Source

class EightCCTest extends StandardTest {
  "8cc vector test" should "print the correct results" in {
    val vector = Paths.get("tests", "scala", "c", "engine", "8cc", "vector.c")
    val buffer = Paths.get("tests", "scala", "c", "engine", "8cc", "buffer.c")
    val main = Paths.get("tests", "scala", "c", "engine", "8cc", "main.c")

    val vectorText = Source.fromFile(vector.toFile, "utf-8").mkString
    val mainText = Source.fromFile(main.toFile, "utf-8").mkString
    val bufferText = Source.fromFile(buffer.toFile, "utf-8").mkString

    val allCode =  Seq(vectorText, bufferText, mainText)

    checkResults2(allCode, includePaths = List(raw"./tests/scala/c/engine/8cc"))
  }
}
