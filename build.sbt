ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.0.0"

ThisBuild / libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.17",
  "org.scalatest" %% "scalatest" % "3.2.17" % "test"
)

ThisBuild / resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"
