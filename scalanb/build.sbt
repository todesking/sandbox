val Scala2_11 = "2.11.12"
val Scala2_12 = "2.12.6"

val versionSetting = Seq(
  version := "0.0.1-SNAPSHOT"
)

val coreCross = Seq(
  scalaVersion := Scala2_11,
  crossScalaVersions := Seq(Scala2_11, Scala2_12)
)
lazy val core = project
  .settings(versionSetting)
  .settings(coreCross)
lazy val test = project
  .dependsOn(core)
  .settings(versionSetting)
  .settings(coreCross)

val sparkCross = Seq(
  scalaVersion := Scala2_11,
  crossScalaVersions := Seq(Scala2_11)
)
lazy val spark = project
  .dependsOn(core)
  .settings(versionSetting)
  .settings(sparkCross)
lazy val sparkTest = project.in(file("spark-test"))
  .dependsOn(spark)
  .settings(versionSetting)
  .settings(sparkCross)
