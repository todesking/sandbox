scalaVersion := "2.12.8"

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
scalacOptions in (Test, console) := { (scalacOptions in (Compile, console)).value }

scalariformSettings(true)
