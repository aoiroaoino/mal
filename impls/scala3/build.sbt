inThisBuild(
  Seq(
    scalaVersion := "3.0.1",
    version := "0.1.0-SNAPSHOT"
  )
)

// for tests
lazy val root = (project in file("."))
  .settings(assemblyJarName := "mal.jar")
  .dependsOn(step0, step1, step2, step3, step4, step5, step6, step7)

lazy val core = project

lazy val step0 = project

lazy val step1 = project
  .dependsOn(core)

lazy val step2 = project
  .dependsOn(core)

lazy val step3 = project
  .dependsOn(core)

lazy val step4 = project
  .dependsOn(core)

lazy val step5 = project
  .dependsOn(core)

lazy val step6 = project
  .dependsOn(core)

lazy val step7 = project
  .dependsOn(core)
