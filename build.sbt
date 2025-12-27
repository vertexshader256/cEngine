name := "cEngine"

lazy val root = (project in file("."))
  .settings(
    name         := "cEngine",
    organization := "com.github.bdwashbu",
    scalaVersion := "2.13.1",
    version      := "0.0.5"
  )

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "tests"

scalaVersion := "2.13.18"
//scalaVersion := "3.7.4"

scalacOptions ++= Seq(
	"-encoding",
	"UTF-8",
	"-target:jvm-1.8",
	"-Xsource:3",
	"-deprecation",
	"-Wunused:imports,privates,locals",
)

//parallelExecution in Test := true
//testOptions in Test += Tests.Argument("-P")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.0" % "test",
	//"org.scalactic" %% "scalactic" % "3.2.19",
	//"org.scalatest" %% "scalatest" % "3.2.19",// % "test"
  "org.anarres" % "jcpp" % "1.4.14"
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