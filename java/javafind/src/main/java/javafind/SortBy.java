package javafind;

public enum SortBy {
    FILEPATH("filepath"),
    FILENAME("filename"),
    FILESIZE("filesize"),
    FILETYPE("filetype"),
    LASTMOD("lastmod");
    MIMETYPE("mimetype"),

    private final String name;

    private SortBy(final String name) {
        this.name = name;
    }

    public static SortBy forName(final String name) {
        String lname = name.trim().toLowerCase();
        switch (lname) {
            case "filename":
            case "name":
                return FILENAME;
            case "filesize":
            case "size":
                return FILESIZE;
            case "filetype":
            case "type":
                return FILETYPE;
            case "lastmod":
                return LASTMOD;
            case "mimetype":
            case "mime":
                return MIMETYPE;
            default:
                return FILEPATH;
        }
    }

    public String toName() {
        return this.name;
    }
}
