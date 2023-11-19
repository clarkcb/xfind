package groovyfind

import groovy.transform.CompileStatic

@CompileStatic
class SortByUtil {

    private static final String NAME = 'name'
    private static final String PATH = 'path'
    private static final String SIZE = 'size'
    private static final String TYPE = 'type'
    private static final String LASTMOD = 'lastmod'

    private SortByUtil() {}

    static SortBy fromName(final String sortByName) {
        switch (sortByName.toLowerCase()) {
            case NAME:
                return SortBy.FILENAME
            case SIZE:
                return SortBy.FILESIZE
            case TYPE:
                return SortBy.FILETYPE
            case LASTMOD:
                return SortBy.LASTMOD
            default:
                return SortBy.FILEPATH
        }
    }

    static String toName(final SortBy sortBy) {
        switch (sortBy) {
            case SortBy.FILENAME:
                return NAME
            case SortBy.FILESIZE:
                return SIZE
            case SortBy.FILETYPE:
                return TYPE
            case SortBy.LASTMOD:
                return LASTMOD
            default:
                return PATH
        }
    }

}
