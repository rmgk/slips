import Dependencies.*
import Settings.*

val commonSettings = Def.settings(
  organization := "de.rmgk.slips",
  scalaVersion_3,
  libraryDependencies ++= List(munit.value, munitScalacheck.value)
)

val logging = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure).settings(
    commonSettings,
    commonCrossBuildVersions,
    libraryDependencies += sourcecode.value,
  )

val chain = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure).settings(
    commonSettings,
  )

val category = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure).settings(
    commonSettings,
  )

val scip = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure).settings(
    commonSettings,
  )

val datalog = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure).settings(
    commonSettings,
    noPublish
  )

val direct = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure).settings(
    commonSettings,
    noPublish
  )

val webview =
  project.settings(
    commonSettings,
    noPublish,
    nativeMode      := "debug", //debug release-fast release-full
    nativeLTO       := "none", // none full thin (thin recommended over full)
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
