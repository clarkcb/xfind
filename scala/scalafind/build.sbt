name := "scalafind"

version := "0.1.0"

scalaVersion := "2.13.5"

scalacOptions += "-target:jvm-11"

initialize := {
  val _ = initialize.value // run the previous initialization
  val required = VersionNumber("11")
  val current = VersionNumber(sys.props("java.specification.version"))
  assert(current == required, s"Unsupported JDK: java.specification.version $current != $required")
}

libraryDependencies ++= Seq(
  "com.googlecode.json-simple" % "json-simple" % "1.1.1",
  "org.apache.commons" % "commons-compress" % "1.20",
  "org.scala-lang.modules" % "scala-xml_2.13" % "1.3.0",
  "org.scalatest" % "scalatest_2.13" % "3.2.6" % Test,
  "junit" % "junit" % "4.13.2" % Test
)

