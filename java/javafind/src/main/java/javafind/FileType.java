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
        final var lName = name.trim().toLowerCase();
        for (final var fileType : FileType.values()) {
            if (fileType.name.equals(lName)) {
                return fileType;
            }
        }
        return FileType.UNKNOWN;
    }

    public String toName() {
        return this.name;
    }
}
