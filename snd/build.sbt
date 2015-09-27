scalaVersion := "2.11.7"

resolvers += "com.todesking" at "http://todesking.github.io/mvn/"

libraryDependencies += "com.todesking" %% "scala-pp" % "0.0.4"

libraryDependencies ++= Seq(
)

scalariformSettings

fork in run := true
