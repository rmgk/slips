import Dependencies.*
import Settings.*

val commonSettings = Def.settings(
  organization := "de.rmgk.slips",
  scalaVersion_3,
  strict
)

lazy val logging = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("logging")).settings(
    name := "logging",
    commonSettings,
    commonCrossBuildVersions,
    libraryDependencies += sourcecode.value,
  )

lazy val chain = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("chain")).settings(
    name := "chain",
    crossScalaVersions := Seq(Versions.scala3),
    commonSettings,
  )
