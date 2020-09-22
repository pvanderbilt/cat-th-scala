ThisBuild / scalaVersion := "2.13.3"
ThisBuild / organization := "com.example"

lazy val hello = (project in file("."))
  .settings(
    name := "ct-scala",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test",
  )

