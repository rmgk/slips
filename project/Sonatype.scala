import sbt.*
import sbt.Keys.*

object Tools {
// use `publishSigned` to publish
// go to https://s01.oss.sonatype.org/#stagingRepositories to move from staging to maven central
  val publishSonatype = Def.settings(
    organization         := "de.rmgk.slips",
    organizationName     := "rmgk",
    organizationHomepage := None,
    homepage             := Some(url("https://github.com/rmgk/slips/")),
    licenses             := List("Apache 2" -> new URI("http://www.apache.org/licenses/LICENSE-2.0.txt").toURL),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/rmgk/slips"),
        "scm:git@github.com:rmgk/slips.git"
      )
    ),
    developers := List(
      Developer(
        id = "ragnar",
        name = "Ragnar Mogk",
        email = "git@rmgk.de",
        url = url("https://github.com/rmgk/")
      )
    ),

    // no binary compatibility for 0.Y.z releases
    versionScheme := Some("strict"),

    // Remove all additional repository other than Maven Central from POM
    pomIncludeRepository := { _ => false },
    publishTo := {
      val nexus = "https://s01.oss.sonatype.org/"
      if (isSnapshot.value) Some("snapshots" at s"${nexus}content/repositories/snapshots")
      else Some("releases" at s"${nexus}service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true
  )
}
