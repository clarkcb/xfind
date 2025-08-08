package scalafind

object Color extends Enumeration {
  type Color = Value
  val RESET: Color = Value("\u001B[0m")
  val BLACK: Color = Value("\u001B[30m")
  val RED: Color = Value("\u001B[31m")
  val GREEN: Color = Value("\u001B[32m")
  val YELLOW: Color = Value("\u001B[33m")
  val BLUE: Color = Value("\u001B[34m")
  val PURPLE: Color = Value("\u001B[35m")
  val CYAN: Color = Value("\u001B[36m")
  val WHITE: Color = Value("\u001B[37m")
}
