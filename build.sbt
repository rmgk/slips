import Settings.*
import Tools.publishSonatype

scalaVersion_3 // define explicit root version, otherwise cross building force defaults a 2.12 build on everything
publish / skip := true

val commonSettings = Def.settings(
  organization := "de.rmgk.slips",
  scalaVersion_3,
  Dependencies.munit, Dependencies.munitCheck,
  explicitNulls(Compile),
  publishSonatype,
)

val logging = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(
    commonSettings,
    Dependencies.sourcecode,
    version    := "0.5.1-SNAPSHOT",
    isSnapshot := true
  )

val chain = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, version := "0.5.0", isSnapshot := false)

val category = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, version := "0.5.0")

val resource = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, version := "0.7.0", isSnapshot := false)

val options = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, version := "0.7.1-SNAPSHOT", isSnapshot := true)
  .dependsOn(resource)

val datalog = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, version := "0.5.0", isSnapshot := true, noPublish)

val delay = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, version := "0.5.0", isSnapshot := false)

val scip = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, version := "0.5.1-SNAPSHOT", isSnapshot := true)
  .dependsOn(delay)

val script = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, version := "0.7.0", isSnapshot := false)
