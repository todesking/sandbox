val versionSetting = Seq(
  version := "0.0.1-SNAPSHOT"
)

val coreCross = Seq(
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.11.12", "2.12.6")
)
lazy val core = project
  .settings(versionSetting)
  .settings(coreCross)
lazy val test = project
  .dependsOn(core)
  .settings(versionSetting)
  .settings(coreCross)

val sparkCross = Seq(
  scalaVersion := "2.11.12",
  crossScalaVersions := Seq("2.11.12")
)
lazy val spark = project
  .dependsOn(core)
  .settings(versionSetting)
  .settings(sparkCross)
lazy val sparkTest = project.in(file("spark-test"))
  .dependsOn(spark)
  .settings(versionSetting)
  .settings(sparkCross)
