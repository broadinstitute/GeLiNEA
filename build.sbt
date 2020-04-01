
scalaVersion     := "2.12.8"

name := "gelinea"
version          := "1.0.0"

lazy val root = (project in file("."))

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

Compile / scalaSource := baseDirectory.value / "src"
Test / scalaSource := baseDirectory.value / "test"
