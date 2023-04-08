val scala3Version = "3.0.1"

ThisBuild / scalaVersion := scala3Version
ThisBuild / organization := "xsearch"

lazy val scalaFind = (project in file("."))
  .settings(
    name := "scalafind",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.googlecode.json-simple" % "json-simple" % "1.1.1",
      "org.apache.commons" % "commons-compress" % "1.21",
      "org.scalactic" %% "scalactic" % "3.2.15",
      "org.scalatest" %% "scalatest" % "3.2.15" % Test,
      "com.novocode" % "junit-interface" % "0.11" % Test
    )
  )
