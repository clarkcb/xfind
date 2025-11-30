package groovyfind

enum Color {
    BLACK("black"),
    RED("red"),
    GREEN("green"),
    YELLOW("yellow"),
    BLUE("blue"),
    MAGENTA("magenta"),
    CYAN("cyan"),
    WHITE("white");

    private final String name

    Color(final String name) {
        this.name = name
    }

    static Color forName(final String name) {
        final var lName = name.trim().toLowerCase()
        for (final Color color : values()) {
            if (color.name == lName) {
                return color
            }
        }
        return BLACK
    }

    ConsoleColor toConsoleColor() {
        return switch (this) {
            case BLACK -> ConsoleColor.BLACK
            case RED -> ConsoleColor.RED
            case GREEN -> ConsoleColor.GREEN
            case YELLOW -> ConsoleColor.YELLOW
            case BLUE -> ConsoleColor.BLUE
            case MAGENTA -> ConsoleColor.MAGENTA
            case CYAN -> ConsoleColor.CYAN
            case WHITE -> ConsoleColor.WHITE
        }
    }

    String getName() {
        return this.name
    }
}
