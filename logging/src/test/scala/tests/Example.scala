package tests

import de.rmgk.logging.{Level, Loggable, Logger}

object Example {

  val log = Logger()

  given Loggable[Int] with
    override def normal(v: Int): String = s"number ${v}"

  log.warn("download failed", 5)


  def main(args: Array[String]): Unit = {
    log.info("This is the server logger")
    ()
  }

}
