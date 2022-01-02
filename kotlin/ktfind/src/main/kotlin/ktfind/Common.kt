package ktfind

/**
 * @author cary on 7/23/16.
 */
fun log(message: String) {
    println(message)
}

fun logError(message: String) {
    println("ERROR: $message")
}

class FindException(err: String) : Exception(err)
