scalaVersion := "2.11.7"

resolvers += "com.todesking" at "http://todesking.github.io/mvn/"

libraryDependencies += "com.todesking" %% "scala-pp" % "0.0.4"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.4"

libraryDependencies ++= Seq(
)

scalariformSettings

fork in run := true

javaOptions in run ++= Seq("-verbose:gc", "-Xloggc:gc.log", "-XX:+PrintGCDetails")
