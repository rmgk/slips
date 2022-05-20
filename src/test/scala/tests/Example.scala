package tests


object Example {

  import de.rmgk.logging.{Level, Logger}

  object Log {

    val common: Logger = Logger(tag = "", level = Level.Trace)
    val Tool: Logger = common.copy(tag = "Tool", level = Level.Info)
    val Main: Logger = common
    val Web: Logger = common.copy(tag = "Web")
    val Store: Logger = common.copy(tag = "IO")
    val Server: Logger = common.copy(tag = "Serv", logPrinter = Logger.tracing)
  }


  Log.Web.warn("download failed")

  object Server {
    import Log.{Server => log}

    log.info("This is the server logger")
  }

  def main(args: Array[String]): Unit = {
    Server
  }

}
