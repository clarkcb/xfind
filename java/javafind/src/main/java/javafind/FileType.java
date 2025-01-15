package javafind;

public enum FileType {
    UNKNOWN("unknown"),
    ARCHIVE("archive"),
    AUDIO("audio"),
    BINARY("binary"),
    CODE("code"),
    FONT("font"),
    IMAGE("image"),
    TEXT("text"),
    VIDEO("video"),
    XML("xml");

    private final String name;

    FileType(final String name) {
        this.name = name;
    }

    public static FileType forName(final String name) {
        final String lname = name.trim().toLowerCase();
        for (final FileType fileType : FileType.values()) {
            if (fileType.name.equals(lname)) {
                return fileType;
            }
        }
        return FileType.UNKNOWN;
    }

    public String toName() {
        return this.name;
    }
}
