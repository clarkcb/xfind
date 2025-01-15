package javafind;

public enum SortBy {
    FILEPATH("filepath"),
    FILENAME("filename"),
    FILESIZE("filesize"),
    FILETYPE("filetype"),
    LASTMOD("lastmod");

    private final String name;

    private SortBy(final String name) {
        this.name = name;
    }

    public static SortBy forName(final String name) {
        String lname = name.trim().toLowerCase();
        return switch (lname) {
            case "filename", "name" -> FILENAME;
            case "filesize", "size" -> FILESIZE;
            case "filetype", "type" -> FILETYPE;
            case "lastmod" -> LASTMOD;
            default -> FILEPATH;
        };
    }

    public String toName() {
        return this.name;
    }
}
