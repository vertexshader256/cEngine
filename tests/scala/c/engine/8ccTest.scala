package scala.c.engine

import java.nio.file.Paths

import scala.c.engine.Interpreter._
import scala.io.Source

class EightCCTest extends StandardTest {
  "8cc vector test" should "print the correct results" in {
    val vector = Paths.get("tests", "scala", "c", "engine", "8cc", "vector.c")
    val set = Paths.get("tests", "scala", "c", "engine", "8cc", "set.c")
    val buffer = Paths.get("tests", "scala", "c", "engine", "8cc", "buffer.c")
    val debug = Paths.get("tests", "scala", "c", "engine", "8cc", "debug.c")
    val error = Paths.get("tests", "scala", "c", "engine", "8cc", "error.c")
    val dict = Paths.get("tests", "scala", "c", "engine", "8cc", "dict.c")
    val map = Paths.get("tests", "scala", "c", "engine", "8cc", "map.c")
    val gen = Paths.get("tests", "scala", "c", "engine", "8cc", "gen.c")
    val cpp = Paths.get("tests", "scala", "c", "engine", "8cc", "cpp.c")
    val lex = Paths.get("tests", "scala", "c", "engine", "8cc", "lex.c")
    val file = Paths.get("tests", "scala", "c", "engine", "8cc", "file.c")
    val parse = Paths.get("tests", "scala", "c", "engine", "8cc", "parse.c")
    val path = Paths.get("tests", "scala", "c", "engine", "8cc", "path.c")
    val encoding = Paths.get("tests", "scala", "c", "engine", "8cc", "encoding.c")
    val main = Paths.get("tests", "scala", "c", "engine", "8cc", "main.c")

    val vectorText = Source.fromFile(vector.toFile, "utf-8").mkString
    val setText = Source.fromFile(set.toFile, "utf-8").mkString
    val mainText = Source.fromFile(main.toFile, "utf-8").mkString
    val debugText = Source.fromFile(debug.toFile, "utf-8").mkString
    val dictText = Source.fromFile(dict.toFile, "utf-8").mkString
    val mapText = Source.fromFile(map.toFile, "utf-8").mkString
    val genText = Source.fromFile(gen.toFile, "utf-8").mkString
    val cppText = Source.fromFile(cpp.toFile, "utf-8").mkString
    val lexText = Source.fromFile(lex.toFile, "utf-8").mkString
    val errorText = Source.fromFile(error.toFile, "utf-8").mkString
    val fileText = Source.fromFile(file.toFile, "utf-8").mkString
    val parseText = Source.fromFile(parse.toFile, "utf-8").mkString
    val pathText = Source.fromFile(path.toFile, "utf-8").mkString
    val encodingText = Source.fromFile(encoding.toFile, "utf-8").mkString
    val bufferText = Source.fromFile(buffer.toFile, "utf-8").mkString

    val allCode =  Seq(vectorText, setText, genText, debugText, encodingText, pathText, parseText, cppText, fileText, lexText, errorText, mapText, dictText, bufferText, mainText)

    checkResults2(allCode, includePaths = List(raw"./tests/scala/c/engine/8cc"))
  }
}
