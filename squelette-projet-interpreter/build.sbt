name := "Interpreter"
version := "0.1"
scalaVersion := "2.13.9"

// Java dependencies
libraryDependencies += "junit" % "junit" % "4.12" % Test
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test exclude("juni", "junit-dep")

// Scala dependencies
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"
