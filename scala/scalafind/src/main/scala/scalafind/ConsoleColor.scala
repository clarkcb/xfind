package scalafind

object ConsoleColor extends Enumeration {
  type ConsoleColor = Value
  val RESET: ConsoleColor = Value("\u001B[0m")
  val BLACK: ConsoleColor = Value("\u001B[0;30m")
  val RED: ConsoleColor = Value("\u001B[0;31m")
  val GREEN: ConsoleColor = Value("\u001B[0;32m")
  val YELLOW: ConsoleColor = Value("\u001B[0;33m")
  val BLUE: ConsoleColor = Value("\u001B[0;34m")
  val MAGENTA: ConsoleColor = Value("\u001B[0;35m")
  val CYAN: ConsoleColor = Value("\u001B[0;36m")
  val WHITE: ConsoleColor = Value("\u001B[0;37m")

  val BOLD_BLACK: ConsoleColor = Value("\u001B[1;30m")
  val BOLD_RED: ConsoleColor = Value("\u001B[1;31m")
  val BOLD_GREEN: ConsoleColor = Value("\u001B[1;32m")
  val BOLD_YELLOW: ConsoleColor = Value("\u001B[1;33m")
  val BOLD_BLUE: ConsoleColor = Value("\u001B[1;34m")
  val BOLD_MAGENTA: ConsoleColor = Value("\u001B[1;35m")
  val BOLD_CYAN: ConsoleColor = Value("\u001B[1;36m")
  val BOLD_WHITE: ConsoleColor = Value("\u001B[1;37m")
}
