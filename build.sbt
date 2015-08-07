name := "scalac-survival-guide"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

licenses += "BSD-3-Clause" -> url("http://opensource.org/licenses/BSD-3-Clause")

fork in run := true