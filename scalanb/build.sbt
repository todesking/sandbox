version := "0.0.1-SNAPSHOT"

lazy val core = project
lazy val test = project.dependsOn(core)
