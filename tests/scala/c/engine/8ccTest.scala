package scala.c.engine

import java.nio.file.Paths

import scala.c.engine.Interpreter._
import scala.io.Source

class EightCCTest extends StandardTest {
  "8cc vector test" should "print the correct results" in {
    val vector = Paths.get("tests", "scala", "c", "engine", "8cc", "vector.c")
    val buffer = Paths.get("tests", "scala", "c", "engine", "8cc", "buffer.c")
    val debug = Paths.get("tests", "scala", "c", "engine", "8cc", "debug.c")
    val error = Paths.get("tests", "scala", "c", "engine", "8cc", "error.c")
    val dict = Paths.get("tests", "scala", "c", "engine", "8cc", "dict.c")
    val map = Paths.get("tests", "scala", "c", "engine", "8cc", "map.c")
    val cpp = Paths.get("tests", "scala", "c", "engine", "8cc", "cpp.c")
    val main = Paths.get("tests", "scala", "c", "engine", "8cc", "main.c")

    val vectorText = Source.fromFile(vector.toFile, "utf-8").mkString
    val mainText = Source.fromFile(main.toFile, "utf-8").mkString
    val debugText = Source.fromFile(debug.toFile, "utf-8").mkString
    val dictText = Source.fromFile(dict.toFile, "utf-8").mkString
    val mapText = Source.fromFile(map.toFile, "utf-8").mkString
    val cppText = Source.fromFile(cpp.toFile, "utf-8").mkString
    val errorText = Source.fromFile(error.toFile, "utf-8").mkString
    val bufferText = Source.fromFile(buffer.toFile, "utf-8").mkString

    val allCode =  Seq(vectorText, debugText, errorText, mapText, dictText, bufferText, mainText)

    checkResults2(allCode, includePaths = List(raw"./tests/scala/c/engine/8cc"))
  }
}
