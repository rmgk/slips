import Dependencies._
import Settings._


ThisBuild / organization := "de.rmgk"
name := "datalog"
inThisBuild(scalaVersion_212)

lazy val core = project
                .in(file("."))
                .settings(
                  name := "datalog",
                  strictCompile,
                  scalatest,
                  scalacheck,
                  cats,
                  scribe
                )
