import Dependencies.*
import Settings.*

val commonSettings = Def.settings(
  organization := "de.rmgk.slips",
  scalaVersion_3,
  libraryDependencies ++= List(munit.value, munitScalacheck.value)
)

lazy val logging = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure).settings(
    commonSettings,
    commonCrossBuildVersions,
    libraryDependencies += sourcecode.value,
  )

lazy val chain = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure).settings(
    commonSettings,
  )

lazy val category = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure).settings(
    commonSettings,
  )

lazy val scip = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure).settings(
    commonSettings,
  )

lazy val datalog = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure).settings(
    commonSettings,
    noPublish
  )
