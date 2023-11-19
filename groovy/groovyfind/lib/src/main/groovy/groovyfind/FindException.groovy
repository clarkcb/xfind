package groovyfind

import groovy.transform.CompileStatic

@CompileStatic
class FindException extends Exception {

    FindException() {
        super()
    }

    FindException(final String msg) {
        super(msg)
    }

    FindException(final String msg, final Exception cause) {
        super(msg, cause)
    }

}
