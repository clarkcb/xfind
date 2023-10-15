package javafind;

public final class DefaultFindSettings {
    private DefaultFindSettings() {
        // inaccessible constructor for utility class
    }

    public static final boolean ARCHIVES_ONLY = false;
    public static final boolean DEBUG = false;
    public static final boolean EXCLUDE_HIDDEN = true;
    public static final boolean INCLUDE_ARCHIVES = false;
    public static final boolean LIST_DIRS = false;
    public static final boolean LIST_FILES = false;
    public static final int MAX_DEPTH = -1;
    public static final int MAX_SIZE = 0;
    public static final int MIN_DEPTH = -1;
    public static final int MIN_SIZE = 0;
    public static final boolean PRINT_USAGE = false;
    public static final boolean PRINT_VERSION = false;
    public static final boolean RECURSIVE = true;
    public static final SortBy SORT_BY = SortBy.FILEPATH;
    public static final boolean SORT_CASE_INSENSITIVE = false;
    public static final boolean SORT_DESCENDING = false;
    public static final boolean VERBOSE = false;
}
