package groovyfind

import groovy.transform.CompileStatic

@CompileStatic
class Logger {

    static void log(final String message) {
        System.out.println(message)
    }

    static void logError(final String message, final boolean colorize = true) {
        if (colorize) {
            System.err.println(Color.BOLD_RED.value + "ERROR: ${message}" + Color.RESET.value)
        } else {
            System.err.println("ERROR: ${message}")
        }
    }

}
