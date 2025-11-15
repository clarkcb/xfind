package scalafind

object Common {
  def log(message: String): Unit = {
    println(message)
  }

  def logError(message: String, colorize: Boolean = true): Unit = {
    if (colorize) {
      System.err.println(Color.BOLD_RED.toString +"ERROR: " + message + Color.RESET.toString)
    } else {
      System.err.println("ERROR: " + message)
    }
  }
}
