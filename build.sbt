organization := "com.agilogy"

name := "uris"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.1.3" % "test"
)

net.virtualvoid.sbt.graph.Plugin.graphSettings
