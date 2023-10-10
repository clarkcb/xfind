package scalafind

object Common {
  def log(message: String): Unit = {
    println(message)
  }

  def logError(message: String): Unit = {
    System.err.println("ERROR: " + message)
  }
}
