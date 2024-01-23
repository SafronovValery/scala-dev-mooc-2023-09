import sbt.Keys.libraryDependencies
import sbt._

object Dependencies {
  lazy val KindProjectorVersion = "0.10.3"
  lazy val ZioVersion = "1.0.4"


  lazy val kindProjector =
    "org.typelevel" %% "kind-projector" % KindProjectorVersion

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.11"

  lazy val cats = Seq(
    "org.typelevel" %% "cats-core" % "2.1.0",
    "org.typelevel" %% "cats-effect" % "3.4.5"
  )

  lazy val zio: Seq[ModuleID] = Seq(
    "dev.zio" %% "zio" % ZioVersion,
    "dev.zio" %% "zio-test" % ZioVersion,
    "dev.zio" %% "zio-test-sbt" % ZioVersion,
    "dev.zio" %% "zio-macros" % ZioVersion
  )

  lazy val zioConfig: Seq[ModuleID] = Seq(
    "dev.zio" %% "zio-config" % "1.0.5",
    "dev.zio" %% "zio-config-magnolia" % "1.0.5",
    "dev.zio" %% "zio-config-typesafe" % "1.0.5"
  )
}
