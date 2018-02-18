
lazy val root = project.in(file(".")).
  aggregate(loggingJVM, loggingJS).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val logging = crossProject.in(file(".")).settings(
  name := "logging",
  organization := "de.rmgk",
  version := "0.2.0",
  scalaVersion := "2.12.4",

  libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.1.4",


  maxErrors := 10,

  compilerOptions,
  bintray,
)

lazy val loggingJVM = logging.jvm
lazy val loggingJS = logging.js


lazy val compilerOptions = Compile / compile / scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-feature",
  "-target:jvm-1.8",
  "-Xlint",
  "-Xfuture",
  //"-Xlog-implicits" ,
  //"-Yno-predef" ,
  //"-Yno-imports" ,
  "-Xfatal-warnings",
  //"-Yinline-warnings" ,
  "-Yno-adapted-args",
  //"-Ywarn-dead-code" ,
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  //"-Ywarn-value-discard" ,
)

/*
* publish procedure copied from:
*   https://github.com/portable-scala/sbt-crossproject/commit/fbe10fe5cee1f545be75a310612b30e520729a0d#diff-6a3371457528722a734f3c51d9238c13
* Have your Bintray credentials stored as
  [documented here](http://www.scala-sbt.org/1.0/docs/Publishing.html#Credentials),
  using realm `Bintray API Realm` and host `api.bintray.com`
* Use `publish` from sbt
* Log in to Bintray and publish the files that were sent
*/
lazy val bintray = Seq(
  publishArtifact in Compile := true,
  publishArtifact in Test := false,
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(
    ScmInfo(
      browseUrl = url("https://github.com/rmgk/logging/"),
      connection = "scm:git:git@github.com:rmgk/logging.git"
    )
  ),
  // Publish to Bintray, without the sbt-bintray plugin
  publishMavenStyle := true,
  publishTo := {
    val proj = moduleName.value
    val ver = version.value
    if (isSnapshot.value) {
      None // Bintray does not support snapshots
    } else {
      val url = new java.net.URL(
        s"https://api.bintray.com/content/rmgk/maven/$proj/$ver")
      val patterns = Resolver.mavenStylePatterns
      Some(Resolver.url("bintray", url)(patterns))
    }
  }
)

