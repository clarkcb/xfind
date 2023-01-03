package javafind;

public class SortByUtil {

    private static final String name = "name";
    private static final String path = "path";
    private static final String type = "type";

    private SortByUtil() {}

    public static SortBy fromName(final String sortByName) {
        String lname = sortByName.toLowerCase();
        if (lname.equals(name)) return SortBy.FILENAME;
        if (lname.equals(type)) return SortBy.FILETYPE;
        return SortBy.FILEPATH;
    }

    public static String toName(final SortBy sortBy) {
        switch (sortBy) {
            case FILENAME:
                return name;
            case FILETYPE:
                return type;
            default:
                return path;
        }
    }
}
