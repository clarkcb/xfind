package javafind;

public class Logger {
    public static void log(final String message) {
        System.out.println(message);
    }

    public static void logError(final String message) {
        System.err.println("ERROR: " + message);
    }
}
