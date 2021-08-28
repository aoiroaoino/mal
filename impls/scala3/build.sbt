inThisBuild(
  Seq(
    scalaVersion := "3.0.1",
    version := "0.1.0-SNAPSHOT"
  )
)

// for tests
lazy val root = (project in file("."))
  .settings(assemblyJarName := "mal.jar")
  .dependsOn(step0, step1)

lazy val core = project

lazy val step0 = project

lazy val step1 = project
  .dependsOn(core)
