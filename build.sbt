import scala.collection.Seq

ThisBuild / version      := "0.0.1-SNAPSHOT"
ThisBuild / scalaVersion := "3.6.1"
ThisBuild / javacOptions ++= Seq("--release", "11")

libraryDependencies ++= Seq(
  "org.apache.poi"                 % "poi"                      % "5.3.0", // java library for working with `.xls` files
  "org.apache.poi"                 % "poi-ooxml"                % "5.3.0",
  "com.lihaoyi"                   %% "upickle"                  % "4.0.2", // json serialization library
  "com.softwaremill.sttp.client4" %% "core"                     % "4.0.0-M19", // http client
  "org.scalatest"                 %% "scalatest"                % "3.2.18"   % Test,
  "org.scalatestplus"             %% "mockito-5-12"             % "3.2.19.0" % Test,
  "org.scalamock"                 %% "scalamock"                % "6.0.0"    % Test,
)

coverageEnabled := true