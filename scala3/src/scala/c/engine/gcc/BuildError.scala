package scala.c.engine.gcc

case class BuildError(problemPath: Seq[ErrorLocation], function: Option[String], errorType: String, error: String)
