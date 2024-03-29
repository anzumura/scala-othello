ThisBuild / organization := "com.github.anzumura"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-explain",
  "-explain-types",
  "-feature",
  "-print-lines",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xmigration"
)
// ThisBuild / logLevel := Level.Debug // for debugging sbt problems

val scalaTestVersion = "3.2.17"

lazy val root = (project in file("."))
  .settings(
    name := "scala-othello",
    libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion
      % Test
  )
