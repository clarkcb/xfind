package javafind;

public enum SortBy {
    FILEPATH("filepath"),
    FILENAME("filename"),
    FILESIZE("filesize"),
    FILETYPE("filetype"),
    LASTMOD("lastmod");

    private final String name;

    SortBy(final String name) {
        this.name = name;
    }

    public static SortBy forName(final String name) {
        final var lName = name.trim().toLowerCase();
        return switch (lName) {
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
