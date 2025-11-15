package groovyfind

enum Color {
    RESET("\u001B[0m"),
    BLACK("\u001B[0;30m"),
    RED("\u001B[0;31m"),
    GREEN("\u001B[0;32m"),
    YELLOW("\u001B[0;33m"),
    BLUE("\u001B[0;34m"),
    MAGENTA("\u001B[0;35m"),
    CYAN("\u001B[0;36m"),
    WHITE("\u001B[0;37m"),

    BOLD_BLACK("\u001B[1;30m"),
    BOLD_RED("\u001B[1;31m"),
    BOLD_GREEN("\u001B[1;32m"),
    BOLD_YELLOW("\u001B[1;33m"),
    BOLD_BLUE("\u001B[1;34m"),
    BOLD_MAGENTA("\u001B[1;35m"),
    BOLD_CYAN("\u001B[1;36m"),
    BOLD_WHITE("\u001B[1;37m");

    private final String value

    Color(final String value) {
        this.value = value
    }

    String getValue() {
        return value
    }
}
