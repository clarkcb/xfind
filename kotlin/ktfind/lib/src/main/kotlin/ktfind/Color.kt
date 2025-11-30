package ktfind

enum class Color(val value: String) {
    BLACK("black"),
    RED("red"),
    GREEN("green"),
    YELLOW("yellow"),
    BLUE("blue"),
    MAGENTA("magenta"),
    CYAN("cyan"),
    WHITE("white");

    fun toConsoleColor(): ConsoleColor {
        return when (this) {
            BLACK -> ConsoleColor.BLACK
            RED -> ConsoleColor.RED
            GREEN -> ConsoleColor.GREEN
            YELLOW -> ConsoleColor.YELLOW
            BLUE -> ConsoleColor.BLUE
            MAGENTA -> ConsoleColor.MAGENTA
            CYAN -> ConsoleColor.CYAN
            WHITE -> ConsoleColor.WHITE
        }
    }

    override fun toString(): String {
        return value
    }
}
