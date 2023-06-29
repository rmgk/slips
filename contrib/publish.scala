#!/usr/bin/env -S scala-cli shebang

//> using scala "3.3.0"
//> using lib "de.rmgk.slips::script:0.4.8"
//> using lib "de.rmgk.slips::options:0.4.7"

import de.rmgk.script.extensions

val projects = Map(
  "category" -> Set("JS", "JVM", "Native"),
  "chain"    -> Set("JS", "JVM", "Native"),
  "datalong" -> Set("JS", "JVM", "Native"),
  "delay"    -> Set("JS", "JVM", "Native"),
  "logging"  -> Set("JS", "JVM", "Native"),
  "options"  -> Set("JS", "JVM", "Native"),
  "resource" -> Set("JS", "JVM", "Native"),
  "scip"     -> Set("JS", "JVM", "Native"),
  "script"   -> Set("JVM", "Native"),
)

@main
def publish(args: String*): Unit = {
  args.foreach: project =>
    projects.get(project) match
      case None => println(s"unknown $project")
      case Some(platforms) =>
        platforms.foreach: platform =>
          val command = s"$project$platform/publishSigned"
          println(command)
          process"sbt --client ${command}".runPrint()
}
