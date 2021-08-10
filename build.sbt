/*val scala3Version = "3.0.1"*/

lazy val root = project
  .in(file("."))
  .settings(
    name := "calculib",
    /*scalaVersion := scala3Version,*/
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % Test
)
