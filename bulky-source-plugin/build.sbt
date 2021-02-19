lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin)
  .settings(
    name := "bulky-source-plugin",
    organization := "sbt.plugins",
    version := "0.1"
  )