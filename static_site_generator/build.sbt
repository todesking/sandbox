organization := "com.todesking"

name := "sciatic"

version := "0.0.0-SNAPSHOT"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
)

scalacOptions ++= Seq(
  "-feature",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Xlint"
)
