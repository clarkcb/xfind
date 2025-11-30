package scalafind

import scalafind.ConsoleColor.ConsoleColor

object Color extends Enumeration {
  type Color = Value
  val BLACK: Color = Value("black")
  val RED: Color = Value("red")
  val GREEN: Color = Value("green")
  val YELLOW: Color = Value("yellow")
  val BLUE: Color = Value("blue")
  val MAGENTA: Color = Value("magenta")
  val CYAN: Color = Value("cyan")
  val WHITE: Color = Value("white")

  def toConsoleColor(color: Color): ConsoleColor = {
    color match {
      case BLACK => ConsoleColor.BLACK
      case RED => ConsoleColor.RED
      case GREEN => ConsoleColor.GREEN
      case YELLOW => ConsoleColor.YELLOW
      case BLUE => ConsoleColor.BLUE
      case MAGENTA => ConsoleColor.MAGENTA
      case CYAN => ConsoleColor.CYAN
      case WHITE => ConsoleColor.WHITE
    }
  }
}
