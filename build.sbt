ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "free-monad-exercise",
    libraryDependencies ++= Seq(
//      "com.github.fd4s" %% "fs2-kafka" % "2.5.0",
      "org.typelevel" %% "cats-effect" % "3.5.0-RC3"
    )
  )
