package scalafind

object Color extends Enumeration {
  type Color = Value
  val RESET: Color = Value("\u001B[0m")
  val BLACK: Color = Value("\u001B[0;30m")
  val RED: Color = Value("\u001B[0;31m")
  val GREEN: Color = Value("\u001B[0;32m")
  val YELLOW: Color = Value("\u001B[0;33m")
  val BLUE: Color = Value("\u001B[0;34m")
  val MAGENTA: Color = Value("\u001B[0;35m")
  val CYAN: Color = Value("\u001B[0;36m")
  val WHITE: Color = Value("\u001B[0;37m")

  val BOLD_BLACK: Color = Value("\u001B[1;30m")
  val BOLD_RED: Color = Value("\u001B[1;31m")
  val BOLD_GREEN: Color = Value("\u001B[1;32m")
  val BOLD_YELLOW: Color = Value("\u001B[1;33m")
  val BOLD_BLUE: Color = Value("\u001B[1;34m")
  val BOLD_MAGENTA: Color = Value("\u001B[1;35m")
  val BOLD_CYAN: Color = Value("\u001B[1;36m")
  val BOLD_WHITE: Color = Value("\u001B[1;37m")
}
