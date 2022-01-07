name := "CRDT_AST"

version := "0.1"

scalaVersion := "2.13.7"

// Adding support for Scala.js

enablePlugins(ScalaJSPlugin)

name := "Scala.js Tutorial"
scalaVersion := "2.13.1" // or any other Scala version >= 2.11.12

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"

// Adding support for writing tests
libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.10" % "test"

// Adding support for parsing
libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.3"

// Adding support for serialization
libraryDependencies += "io.suzaku" %%% "boopickle" % "1.4.0"