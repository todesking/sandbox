addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

// scalacOptions += "-Ymacro-debug-lite"
scalacOptions += "-Yrangepos"

libraryDependencies += "org.apache.spark" %% "spark-sql" % "2.0.0" % "provided"
