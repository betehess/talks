name := "cata-visitor-adt"

version := "1.0"

scalaVersion := "2.11.1"

scalacOptions ++= Seq("-feature")

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

initialize := {
  val _ = initialize.value
  if (sys.props("java.specification.version") != "1.8")
    sys.error("Java 8 is required for this project.")
}
