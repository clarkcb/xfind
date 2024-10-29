val scala3Version = "3.7.4"

ThisBuild / scalaVersion := scala3Version
ThisBuild / organization := "xsearch"

lazy val scalaFind = (project in file("."))
  .settings(
    name := "scalafind",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.json" % "json" % "20251224",
      "org.apache.commons" % "commons-compress" % "1.28.0",
      "org.scalactic" %% "scalactic" % "3.2.19",

      "org.xerial" % "sqlite-jdbc" % "3.47.0.0",
      // excluding from sqlite-jdbc and added newer slf4j versions didn't work
      // I still saw warning messages, so I added slf4j-nop that has the same
      // version as the one included in sqlite-jdbc to suppress them
      // "org.xerial" % "sqlite-jdbc" % "3.46.0.0" exclude("org.slf4j", "slf4j-api"),
      // "org.slf4j" % "slf4j-api" % "2.0.12",
      // "org.slf4j" % "slf4j-simple" % "2.0.13",
      "org.slf4j" % "slf4j-nop" % "1.7.36",
      // "org.slf4j" % "slf4j-nop" % "2.0.13",

      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "com.github.sbt" % "junit-interface" % "0.13.3" % Test
    )
  )

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}
