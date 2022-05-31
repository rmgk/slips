import Dependencies.*
import Settings.*

val commonSettings = Def.settings(
  organization := "de.rmgk.slips",
  scalaVersion_3,
  libraryDependencies ++= List(munit.value, munitScalacheck.value)
)

lazy val logging = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("logging")).settings(
    name := "logging",
    commonSettings,
    commonCrossBuildVersions,
    libraryDependencies += sourcecode.value,
  )

lazy val chain = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("chain")).settings(
    name := "chain",
    commonSettings,
  )

lazy val category = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("category")).settings(
    name := "category",
    commonSettings,
  )

lazy val scip = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("scip")).settings(
    name := "scip",
    commonSettings,
  )

lazy val datalog = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("datalog")).settings(
    name := "datalog",
    commonSettings,
    noPublish
  )
