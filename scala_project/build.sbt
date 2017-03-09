scalaVersion := "2.11.8"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Xfuture",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
)
scalacOptions in (Compile, console) ~= {_.filterNot(_ == "-Ywarn-unused-import")}
scalacOptions in (Test, console) <<= (scalacOptions in (Compile, console))

scalariformSettings
