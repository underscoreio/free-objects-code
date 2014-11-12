val scalaz = "org.scalaz" %% "scalaz-core" % "7.1.0"
val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.11.6" % "test"

val settings = Seq(
  scalaVersion := "2.11.3",
  libraryDependencies ++= Seq(scalaz, scalaCheck)
)

lazy val monoid = project.settings(settings:_*)

lazy val monad = project.settings(settings:_*)
