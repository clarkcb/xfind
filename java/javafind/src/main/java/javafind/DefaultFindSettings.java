package javafind;

public final class DefaultFindSettings {
    private DefaultFindSettings() {
        // inaccessible constructor for utility class
    }

    public static final boolean ARCHIVESONLY = false;
    public static final boolean DEBUG = false;
    public static final boolean EXCLUDEHIDDEN = true;
    public static final boolean INCLUDEARCHIVES = false;
    public static final boolean LISTDIRS = false;
    public static final boolean LISTFILES = false;
    public static final int MAXSIZE = 0;
    public static final int MINSIZE = 0;
    public static final boolean PRINTUSAGE = false;
    public static final boolean PRINTVERSION = false;
    public static final boolean RECURSIVE = true;
    public static final SortBy SORT_BY = SortBy.FILEPATH;
    public static final boolean SORT_CASEINSENSITIVE = false;
    public static final boolean SORT_DESCENDING = false;
    public static final boolean VERBOSE = false;
}
