import Dependencies.*
import Settings.*
import Tools.publishSonatype

scalaVersion_3 // define explicit root version, otherwise cross building force defaults a 2.12 build on everything
publish / skip := true

val commonSettings = Def.settings(
  organization := "de.rmgk.slips",
  scalaVersion_3,
  libraryDependencies ++= List(munit.value, munitCheck.value),
  explicitNulls(Compile),
  publishSonatype,
)

val logging = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(
    commonSettings,
    libraryDependencies += sourcecode.value,
    version := "0.5.0"
  )

val chain = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, version := "0.5.0")

val category = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, version := "0.5.0")

val options = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, libraryDependencies += scopt.value, version := "0.5.0")
  .jsSettings(
    // seems to be required to run JS tests on node
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )

val datalog = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, noPublish, version := "0.5.0")

val resource = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, version := "0.5.0")

val delay = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, version := "0.5.0")

val scip = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, version := "0.5.0")
  .dependsOn(delay)

val script = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings, version := "0.5.0")

val webview =
  project.settings(
    commonSettings,
    noPublish,
    nativeMode      := "debug", // debug release-fast release-full
    nativeLTO       := "none",  // none full thin (thin recommended over full)
    nativeLinkStubs := true,
    nativeCompileOptions ++= fromCommand("pkg-config", "--cflags", "gtk+-3.0", "webkit2gtk-4.0"),
    nativeLinkingOptions ++= fromCommand("pkg-config", "--libs", "gtk+-3.0", "webkit2gtk-4.0")
  ).enablePlugins(ScalaNativePlugin)

def fromCommand(args: String*): List[String] = {
  val process = new ProcessBuilder(args: _*).start()
  process.waitFor()
  val res = new String(process.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
  res.split(raw"\s+").toList
}

val benchmark = project
  .enablePlugins(JmhPlugin)
  .settings(commonSettings, noPublish)
  .dependsOn(scip.jvm)
