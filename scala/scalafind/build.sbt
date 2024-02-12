val scala3Version = "3.2.2"

ThisBuild / scalaVersion := scala3Version
ThisBuild / organization := "xsearch"

lazy val scalaFind = (project in file("."))
  .settings(
    name := "scalafind",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.json" % "json" % "20240205",
      "org.apache.commons" % "commons-compress" % "1.25.0",
      "org.scalactic" %% "scalactic" % "3.2.17",
      "org.scalatest" %% "scalatest" % "3.2.17" % Test,
      "com.github.sbt" % "junit-interface" % "0.13.3" % Test
    )
  )
