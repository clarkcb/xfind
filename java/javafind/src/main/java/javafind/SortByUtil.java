package javafind;

public class SortByUtil {

    private static final String name = "name";
    private static final String path = "path";
    private static final String size = "size";
    private static final String type = "type";
    private static final String lastmod = "lastmod";

    private SortByUtil() {}

    public static SortBy fromName(final String sortByName) {
        var lname = sortByName.toLowerCase();
        if (lname.equals(name)) return SortBy.FILENAME;
        if (lname.equals(size)) return SortBy.FILESIZE;
        if (lname.equals(type)) return SortBy.FILETYPE;
        if (lname.equals(lastmod)) return SortBy.LASTMOD;
        return SortBy.FILEPATH;
    }

    public static String toName(final SortBy sortBy) {
        switch (sortBy) {
            case FILENAME:
                return name;
            case FILESIZE:
                return size;
            case FILETYPE:
                return type;
            case LASTMOD:
                return lastmod;
            default:
                return path;
        }
    }
}
