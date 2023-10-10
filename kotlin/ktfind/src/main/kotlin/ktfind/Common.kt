package ktfind

/**
 * @author cary on 7/23/16.
 */
fun log(message: String) {
    println(message)
}

fun logError(message: String) {
    System.err.println("ERROR: $message")
}

class FindException(err: String) : Exception(err)
