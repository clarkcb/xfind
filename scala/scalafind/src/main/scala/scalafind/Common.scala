package scalafind

object Common {
  def log(message: String): Unit = {
    println(message)
  }

  def logError(message: String, colorize: Boolean = true): Unit = {
    if (colorize) {
      System.err.println(ConsoleColor.BOLD_RED.toString +"ERROR: " + message + ConsoleColor.RESET.toString)
    } else {
      System.err.println("ERROR: " + message)
    }
  }
}
