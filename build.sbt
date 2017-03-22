name := "astViewer"

lazy val root = (project in file(".")).enablePlugins(PlayScala, LauncherJarPlugin)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  filters,
  cache,
  ws,
  "com.github.pathikrit" %% "better-files" % "2.16.0",
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.0-RC1" % Test
)

testOptions in Test += Tests.Argument("-P")