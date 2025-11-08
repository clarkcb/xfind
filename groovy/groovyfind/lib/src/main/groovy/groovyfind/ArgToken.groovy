package groovyfind

import groovy.transform.CompileStatic

@CompileStatic
class ArgToken {
    final String name
    final ArgTokenType type
    final Object value

    ArgToken(final String name, final ArgTokenType type, final Object value) {
        this.name = name
        this.type = type
        this.value = value
    }
}
