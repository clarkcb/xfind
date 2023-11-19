package groovyfind

import groovy.transform.CompileStatic

@CompileStatic
class Logger {

    static void log(final String message) {
        System.out.println(message)
    }

    static void logError(final String message) {
        System.err.println("ERROR: ${message}")
    }

}
