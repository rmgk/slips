import Dependencies.*
import Settings.*

lazy val logging =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("logging")).settings(
      name         := "logging",
      organization := "de.rmgk",
      scalaVersion_3,
      commonCrossBuildVersions,
      libraryDependencies += sourcecode.value,
      strict
    )

lazy val loggingJS     = logging.js
lazy val loggingJVM    = logging.jvm
lazy val loggingNative = logging.native
