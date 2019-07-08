name := "rocket-chip-config-plugin"
version := "0.1.0-SNAPSHOT"
organization := "com.galois.besspin"

scalaVersion := "2.12.7"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.12.7"
resourceDirectory in Compile := baseDirectory.value / "resources"
