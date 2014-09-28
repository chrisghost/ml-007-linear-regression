name := """ml-007"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "org.scalanlp" % "breeze_2.10" % "0.9",
  "org.scalanlp" % "breeze-viz_2.10" % "0.9",
  "org.scalanlp" % "breeze-natives_2.10" % "0.9"
)
