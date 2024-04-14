val scala3Version = "3.4.1"

ThisBuild / scalaVersion := scala3Version
ThisBuild / organization := "xsearch"

lazy val scalaFind = (project in file("."))
  .settings(
    name := "scalafind",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.json" % "json" % "20240303",
      "org.apache.commons" % "commons-compress" % "1.26.1",
      "org.scalactic" %% "scalactic" % "3.2.18",
      "org.scalatest" %% "scalatest" % "3.2.18" % Test,
      "com.github.sbt" % "junit-interface" % "0.13.3" % Test
    )

  )

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}
