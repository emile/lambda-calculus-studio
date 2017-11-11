enablePlugins(ScalaJSPlugin)

name := "parser"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "com.lihaoyi" %%% "fastparse" % "0.4.4"

libraryDependencies += "io.monix" %%% "minitest" % "1.1.0" % "test"

libraryDependencies += "com.thoughtworks.binding" %%% "binding" % "latest.release"

libraryDependencies += "com.thoughtworks.binding" %%% "dom" % "latest.release"

libraryDependencies += "com.thoughtworks.binding" %%% "route" % "latest.release"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

testFrameworks += new TestFramework("minitest.runner.Framework")

//scalaJSUseMainModuleInitializer := true

