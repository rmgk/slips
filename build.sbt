import Dependencies.*
import Settings.*

lazy val root = project.in(file(".")).aggregate(logging.js, logging.jvm, logging.native).settings(
  publish      := {},
  publishLocal := {}
)
lazy val logging =
  crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Pure).in(file(".")).settings(
    name         := "logging",
    organization := "de.rmgk",
    scalaVersion_3,
    commonCrossBuildVersions,
    libraryDependencies += sourcecode.value,
    strict
  )
