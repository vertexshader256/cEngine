name := "cEngine"

lazy val scala3Module = (project in file("scala3"))
	.settings(
		scalaVersion := "3.7.4",
		Compile / scalaSource := baseDirectory.value / "src",
		Test / scalaSource := baseDirectory.value / "tests",
		libraryDependencies ++= Seq("org.anarres" % "jcpp" % "1.4.14", "org.scalactic" %% "scalactic" % "3.2.19", "org.scalatest" %% "scalatest" % "3.2.19")
	)

lazy val root = (project in file("."))
	.aggregate(scala3Module)

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