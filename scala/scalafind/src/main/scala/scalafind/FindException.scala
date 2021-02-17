package scalafind

class FindException(val message: String) extends Exception {
  override def getMessage : String = message
}
