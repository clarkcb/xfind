package groovyfind

enum SortBy {
    FILEPATH('filepath'),
    FILENAME('filename'),
    FILESIZE('filesize'),
    FILETYPE('filetype'),
    LASTMOD('lastmod');

    private final String name

    SortBy(final String name) {
        this.name = name
    }

    static SortBy forName(final String name) {
        String lname = name.trim().toLowerCase()
        switch (lname) {
            case 'filename':
            case 'name':
                return FILENAME
            case 'filesize':
            case 'size':
                return FILESIZE
            case 'filetype':
            case 'type':
                return FILETYPE
            case 'lastmod':
                return LASTMOD
            default:
                return FILEPATH
        }
    }

    String toName() {
        return name
    }
}
