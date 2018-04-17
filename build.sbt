scalaVersion := "2.12.4"

lazy val uno = project
  .in(file("."))
  .settings(
    name := "uno",
	organization := "tech.dougie",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.4" % "test,it"
    ),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-Xfatal-warnings"
    ),
    organizationName := "Zachary Graziano",
    startYear := Some(2018),
    licenses += ("MIT", new URL("file://LICENSE")),
    bintrayRepository := "dougietech",
    Defaults.itSettings
  )
  .configs(IntegrationTest)
