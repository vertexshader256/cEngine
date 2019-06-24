name := "cEngine"

lazy val root = (project in file("."))
  .settings(
    name         := "cEngine",
    organization := "com.github.bdwashbu",
    scalaVersion := "2.13.0",
    version      := "0.0.5"
  )

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "tests"

scalaVersion := "2.13.0"

//parallelExecution in Test := true
//testOptions in Test += Tests.Argument("-P")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)

//assemblyMergeStrategy in assembly := {
//  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
//  case x => MergeStrategy.first
//}

//artifact in (Compile, assembly) := {
//  val art = (artifact in (Compile, assembly)).value
//  art.copy(`classifier` = Some("assembly"))
//}

//addArtifact(artifact in (assembly), assembly)

publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

//testOptions in Test += Tests.Argument("-P")