import sbt._
import Keys._

object ExamplesBuild extends Build {

  override val settings = super.settings ++ Seq(
    scalaVersion := "2.11.6"
  )

  lazy val examples = Project("Examples", file("."))
    .settings (scalacOptions ++= Seq(
	"-feature",
	"-language:existentials",
	"-deprecation"
    ))
    .settings(libraryDependencies ++= Seq(
      "com.cra.figaro" %% "figaro" % "3.3.0.0"
    ))
}
