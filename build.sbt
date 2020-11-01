ThisBuild / scalaVersion := "2.13.3"
ThisBuild / organization := "com.example"

lazy val category = (project in file("."))
  .settings(
    name := "ct-scala",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test",
  )

scalacOptions := Seq("-unchecked", "-deprecation")

Global / onChangedBuildSource := ReloadOnSourceChanges
ThisBuild  / watchBeforeCommand := Watch.clearScreen

// exclude emacs temp files from triggering recompilation
excludeFilter := ".#*"
