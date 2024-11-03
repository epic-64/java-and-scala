import scala.collection.Seq

ThisBuild / version      := "0.0.1-SNAPSHOT"
ThisBuild / scalaVersion := "3.6.1"
ThisBuild / javacOptions ++= Seq("--release", "11")

libraryDependencies ++= Seq(
  "com.lihaoyi"                   %% "upickle"      % "4.0.2",
  "com.softwaremill.sttp.client4" %% "core"         % "4.0.0-M19",
  "org.scalatest"                 %% "scalatest"    % "3.2.18"   % Test,
  "org.scalatestplus"             %% "mockito-5-12" % "3.2.19.0" % Test,
  "org.scalamock"                 %% "scalamock"    % "6.0.0"    % Test,
)
