ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaLearning",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "requests" % "0.8.0",
      "com.lihaoyi" %% "os-lib" % "0.9.1",
      "org.jsoup" % "jsoup" % "1.17.2",
      "ch.qos.logback" % "logback-classic" % "1.4.14",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
    )
  )
