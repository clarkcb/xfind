package ktfind

/**
 * @author cary on 7/23/16.
 */
fun log(message: String) {
    println(message)
}

fun logError(message: String, colorize: Boolean = true) {
    if (colorize) {
        System.err.println(ConsoleColor.BOLD_RED.value +"ERROR: $message" + ConsoleColor.RESET.value)
    } else {
        System.err.println("ERROR: $message")
    }
}

enum class FindError(val message: String) {
    STARTPATH_NOT_DEFINED("Startpath not defined"),
    STARTPATH_NOT_READABLE("Startpath not readable"),
    STARTPATH_NOT_FOUND("Startpath not found"),
    INVALID_RANGE_FOR_MINDEPTH_AND_MAXDEPTH("Invalid range for mindepth and maxdepth"),
    INVALID_RANGE_FOR_MINLASTMOD_AND_MAXLASTMOD("Invalid range for minlastmod and maxlastmod"),
    INVALID_RANGE_FOR_MINSIZE_AND_MAXSIZE("Invalid range for minsize and maxsize"),
    STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS("Startpath does not match find settings");

    override fun toString(): String {
        return message
    }
}

class FindException(err: String) : Exception(err)
