val scala3Version = "3.2.2"

ThisBuild / scalaVersion := scala3Version
ThisBuild / organization := "xsearch"

lazy val scalaFind = (project in file("."))
  .settings(
    name := "scalafind",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.json" % "json" % "20231013",
      "org.apache.commons" % "commons-compress" % "1.21",
      "org.scalactic" %% "scalactic" % "3.2.16",
      "org.scalatest" %% "scalatest" % "3.2.15" % Test,
      "com.novocode" % "junit-interface" % "0.11" % Test
    )
  )
