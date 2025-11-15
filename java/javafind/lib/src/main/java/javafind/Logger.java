package javafind;

public class Logger {
    public static void log(final String message) {
        System.out.println(message);
    }

    public static void logError(final String message) {
        logError(message, true);
    }

    public static void logError(final String message, final boolean colorize) {
        if (colorize) {
            System.err.println(ConsoleColor.BOLD_RED.getValue() + "ERROR: " + message + ConsoleColor.RESET.getValue());
        } else {
            System.err.println("ERROR: " + message);
        }
    }
}
