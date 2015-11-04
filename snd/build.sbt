scalaVersion := "2.11.7"

resolvers += "com.todesking" at "http://todesking.github.io/mvn/"

libraryDependencies += "com.todesking" %% "scala-pp" % "0.0.4"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.4"

libraryDependencies ++= Seq(
)

lazy val fastFunctions = project in file("./fast_functions")

lazy val root = project.in(file(".")).dependsOn(fastFunctions)


scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

scalariformSettings

fork in run := true

javaOptions in run ++= Seq("-server", "-verbose:gc", "-Xloggc:gc.log", "-XX:+PrintGCDetails", "-agentlib:hprof=cpu=samples,depth=20")
