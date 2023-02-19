/*******************************************************************************
FindSettings

Class to encapsulate find settings

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.*;
import java.util.regex.Pattern;

public class FindSettings {

    private static final int INITIAL_SET_CAPACITY = 4;

    private boolean archivesOnly;
    private boolean debug;
    private boolean excludeHidden;
    private final Set<String> inArchiveExtensions;
    private final Set<Pattern> inArchiveFilePatterns;
    private final Set<Pattern> inDirPatterns;
    private final Set<String> inExtensions;
    private final Set<Pattern> inFilePatterns;
    private final Set<FileType> inFileTypes;
    private boolean includeArchives;
    private boolean listDirs;
    private boolean listFiles;
    private LocalDateTime maxLastMod;
    private int maxSize;
    private LocalDateTime minLastMod;
    private int minSize;
    private final Set<String> outArchiveExtensions;
    private final Set<Pattern> outArchiveFilePatterns;
    private final Set<Pattern> outDirPatterns;
    private final Set<String> outExtensions;
    private final Set<Pattern> outFilePatterns;
    private final Set<FileType> outFileTypes;
    private Set<String> paths;
    private boolean printUsage;
    private boolean printVersion;
    private boolean recursive;
    private SortBy sortBy;
    private boolean sortCaseInsensitive;
    private boolean sortDescending;
    private boolean verbose;

    public FindSettings() {
        this.archivesOnly = DefaultSettings.ARCHIVESONLY;
        this.debug = DefaultSettings.DEBUG;
        this.excludeHidden = DefaultSettings.EXCLUDEHIDDEN;
        this.inArchiveExtensions = new HashSet<>(INITIAL_SET_CAPACITY);
        this.inArchiveFilePatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.inDirPatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.inExtensions = new HashSet<>(INITIAL_SET_CAPACITY);
        this.inFilePatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.inFileTypes = new HashSet<>(INITIAL_SET_CAPACITY);
        this.includeArchives = DefaultSettings.INCLUDEARCHIVES;
        this.listDirs = DefaultSettings.LISTDIRS;
        this.listFiles = DefaultSettings.LISTFILES;
        this.maxLastMod = null;
        this.maxSize = DefaultSettings.MAXSIZE;
        this.minLastMod = null;
        this.minSize = DefaultSettings.MINSIZE;
        this.outArchiveExtensions = new HashSet<>(INITIAL_SET_CAPACITY);
        this.outArchiveFilePatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.outDirPatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.outExtensions = new HashSet<>(INITIAL_SET_CAPACITY);
        this.outFilePatterns = new HashSet<>(INITIAL_SET_CAPACITY);
        this.outFileTypes = new HashSet<>(INITIAL_SET_CAPACITY);
        this.paths = new HashSet<>(INITIAL_SET_CAPACITY);
        this.printUsage = DefaultSettings.PRINTUSAGE;
        this.printVersion = DefaultSettings.PRINTVERSION;
        this.recursive = DefaultSettings.RECURSIVE;
        this.sortBy = DefaultSettings.SORT_BY;
        this.sortCaseInsensitive = DefaultSettings.SORT_CASEINSENSITIVE;
        this.sortDescending = DefaultSettings.SORT_DESCENDING;
        this.verbose = DefaultSettings.VERBOSE;
    }

    public final Set<String> getPaths() {
        return this.paths;
    }

    public final void setPaths(final Set<String> paths) {
        this.paths = paths;
    }

    public final void addPath(final String path) {
        this.paths.add(path);
    }

    public final boolean getArchivesOnly() {
        return this.archivesOnly;
    }

    public final void setArchivesOnly(final boolean archivesOnly) {
        this.archivesOnly = archivesOnly;
        if (archivesOnly) {
            this.includeArchives = true;
        }
    }

    public final boolean getDebug() {
        return this.debug;
    }

    public final void setDebug(final boolean debug) {
        this.debug = debug;
        if (debug) {
            this.verbose = true;
        }
    }

    public final boolean getExcludeHidden() {
        return this.excludeHidden;
    }

    public final void setExcludeHidden(final boolean excludeHidden) {
        this.excludeHidden = excludeHidden;
    }

    public final boolean getIncludeArchives() {
        return this.includeArchives;
    }

    public final void setIncludeArchives(final boolean includeArchives) {
        this.includeArchives = includeArchives;
    }

    public final boolean getListDirs() {
        return this.listDirs;
    }

    public final void setListDirs(final boolean listDirs) {
        this.listDirs = listDirs;
    }

    public final boolean getListFiles() {
        return this.listFiles;
    }

    public final void setListFiles(final boolean listFiles) {
        this.listFiles = listFiles;
    }

    public LocalDateTime getMaxLastMod() {
        return maxLastMod;
    }

    private LocalDateTime getLastModFromString(String lastModString) {
        LocalDateTime lastMod = null;
        try {
            lastMod = LocalDateTime.parse(lastModString);
        } catch (DateTimeParseException e) {
            try {
                LocalDate maxLastModDate = LocalDate.parse(lastModString, DateTimeFormatter.ISO_LOCAL_DATE);
                lastMod = maxLastModDate.atTime(0, 0, 0);
            } catch (DateTimeParseException e2) {
                System.out.println("Unable to parse lastModString");
            }
        }
        return lastMod;
    }

    public void setMaxLastMod(String maxLastModString) {
        this.maxLastMod = getLastModFromString(maxLastModString);
    }

    public void setMaxLastMod(LocalDateTime maxLastMod) {
        this.maxLastMod = maxLastMod;
    }

    public int getMaxSize() {
        return maxSize;
    }

    public void setMaxSize(int maxSize) {
        this.maxSize = maxSize;
    }

    public LocalDateTime getMinLastMod() {
        return minLastMod;
    }

    public void setMinLastMod(String minLastModString) {
        this.minLastMod = getLastModFromString(minLastModString);
    }

    public void setMinLastMod(LocalDateTime minLastMod) {
        this.minLastMod = minLastMod;
    }

    public int getMinSize() {
        return minSize;
    }

    public void setMinSize(int minSize) {
        this.minSize = minSize;
    }

    public final boolean getPrintUsage() {
        return this.printUsage;
    }

    public final void setPrintUsage(final boolean printUsage) {
        this.printUsage = printUsage;
    }

    public final boolean getPrintVersion() {
        return this.printVersion;
    }

    public final void setPrintVersion(final boolean printVersion) {
        this.printVersion = printVersion;
    }

    public final boolean getRecursive() {
        return this.recursive;
    }

    public final void setRecursive(final boolean recursive) {
        this.recursive = recursive;
    }

    public final SortBy getSortBy() {
        return sortBy;
    }

    public final void setSortBy(final SortBy sortBy) {
        this.sortBy = sortBy;
    }

    public final boolean getSortCaseInsensitive() {
        return this.sortCaseInsensitive;
    }

    public final void setSortCaseInsensitive(final boolean sortCaseInsensitive) {
        this.sortCaseInsensitive = sortCaseInsensitive;
    }

    public final boolean getSortDescending() {
        return this.sortDescending;
    }

    public final void setSortDescending(final boolean sortDescending) {
        this.sortDescending = sortDescending;
    }

    public final boolean getVerbose() {
        return this.verbose;
    }

    public final void setVerbose(final boolean verbose) {
        this.verbose = verbose;
    }

    // could be a comma-separated list
    private static void addExtensions(Set<String> set, final String exts) {
        addExtensions(set, Arrays.asList(exts.split(",")));
    }

    private static void addExtensions(Set<String> set, final List<String> exts) {
        for (String x : exts) {
            if (!x.isEmpty()) {
                set.add(x.toLowerCase());
            }
        }
    }

    public final Set<String> getInExtensions() {
        return this.inExtensions;
    }

    public final void addInExtension(final String ext) {
        addExtensions(this.inExtensions, ext);
    }

    public final Set<String> getOutExtensions() {
        return this.outExtensions;
    }

    public final void addOutExtension(final String ext) {
        addExtensions(this.outExtensions, ext);
    }

    public final Set<String> getInArchiveExtensions() {
        return this.inArchiveExtensions;
    }

    public final void addInArchiveExtension(final String ext) {
        addExtensions(this.inArchiveExtensions, ext);
    }

    public final Set<String> getOutArchiveExtensions() {
        return this.outArchiveExtensions;
    }

    public final void addOutArchiveExtension(final String ext) {
        addExtensions(this.outArchiveExtensions, ext);
    }

    private static void addPattern(Set<Pattern> set, final String pattern) {
        set.add(Pattern.compile(pattern));
    }

    public final Set<Pattern> getInDirPatterns() {
        return this.inDirPatterns;
    }

    public final void addInDirPattern(final String pattern) {
        addPattern(this.inDirPatterns, pattern);
    }

    public final Set<Pattern> getOutDirPatterns() {
        return this.outDirPatterns;
    }

    public final void addOutDirPattern(final String pattern) {
        addPattern(this.outDirPatterns, pattern);
    }

    public final Set<Pattern> getInFilePatterns() {
        return this.inFilePatterns;
    }

    public final void addInFilePattern(final String pattern) {
        addPattern(this.inFilePatterns, pattern);
    }

    public final Set<Pattern> getOutFilePatterns() {
        return this.outFilePatterns;
    }

    public final void addOutFilePattern(final String pattern) {
        addPattern(this.outFilePatterns, pattern);
    }

    public final Set<FileType> getInFileTypes() {
        return this.inFileTypes;
    }

    public final Set<FileType> getOutFileTypes() {
        return this.outFileTypes;
    }

    public final Set<Pattern> getInArchiveFilePatterns() {
        return this.inArchiveFilePatterns;
    }

    public final void addInArchiveFilePattern(final String pattern) {
        addPattern(this.inArchiveFilePatterns, pattern);
    }

    public final Set<Pattern> getOutArchiveFilePatterns() {
        return this.outArchiveFilePatterns;
    }

    public final void addOutArchiveFilePattern(final String pattern) {
        addPattern(this.outArchiveFilePatterns, pattern);
    }

    // could be a comma-separated list
    private static void addFileTypes(Set<FileType> set, final String fts) {
        addFileTypes(set, Arrays.asList(fts.split(",")));
    }

    private static void addFileTypes(Set<FileType> set, final List<String> fts) {
        for (String ft : fts) {
            if (!ft.isEmpty()) {
                set.add(FileTypes.fromName(ft));
            }
        }
    }

    public final void addInFileType(final String ft) {
        addFileTypes(this.inFileTypes, ft);
    }

    public final void addOutFileType(final String ft) {
        addFileTypes(this.outFileTypes, ft);
    }

    public boolean needStat() {
        return this.sortBy.equals(SortBy.FILESIZE) || this.sortBy.equals(SortBy.LASTMOD) ||
                this.maxLastMod != null || this.minLastMod != null ||
                this.maxSize > 0 || this.minSize > 0;
    }

    private static String stringSetToString(final Set<String> set) {
        StringBuilder sb = new StringBuilder("[");
        int elemCount = 0;
        for (String s : set) {
            if (elemCount > 0) {
                sb.append(", ");
            }
            sb.append("\"").append(s).append("\"");
            elemCount++;
        }
        sb.append("]");
        return sb.toString();
    }

    private static String patternSetToString(final Set<Pattern> set) {
        StringBuilder sb = new StringBuilder("[");
        int elemCount = 0;
        for (Pattern p : set) {
            if (elemCount > 0) {
                sb.append(", ");
            }
            sb.append("\"").append(p.toString()).append("\"");
            elemCount++;
        }
        sb.append("]");
        return sb.toString();
    }

    private static String fileTypeSetToString(final Set<FileType> set) {
        StringBuilder sb = new StringBuilder("[");
        int elemCount = 0;
        for (FileType ft : set) {
            if (elemCount > 0) {
                sb.append(", ");
            }
            sb.append(ft.name());
            elemCount++;
        }
        sb.append("]");
        return sb.toString();
    }

    private static String localDateTimeToString(final LocalDateTime dt) {
        if (dt == null) {
            return "0";
        }
        return String.format("\"%s\"", dt);
    }

    public final String toString() {
        return "FindSettings("
                + "archivesOnly: " + this.archivesOnly
                + ", debug: " + this.debug
                + ", excludeHidden: " + this.excludeHidden
                + ", inArchiveExtensions: " + stringSetToString(this.inArchiveExtensions)
                + ", inArchiveFilePatterns: " + patternSetToString(this.inArchiveFilePatterns)
                + ", inDirPatterns: " + patternSetToString(this.inDirPatterns)
                + ", inExtensions: " + stringSetToString(this.inExtensions)
                + ", inFilePatterns: " + patternSetToString(this.inFilePatterns)
                + ", inFileTypes: " + fileTypeSetToString(this.inFileTypes)
                + ", includeArchives: " + this.includeArchives
                + ", listDirs: " + this.listDirs
                + ", listFiles: " + this.listFiles
                + ", maxLastMod: " + localDateTimeToString(this.maxLastMod)
                + ", maxSize: " + this.maxSize
                + ", minLastMod: " + localDateTimeToString(this.minLastMod)
                + ", minSize: " + this.minSize
                + ", outArchiveExtensions: " + stringSetToString(this.outArchiveExtensions)
                + ", outArchiveFilePatterns: " + patternSetToString(this.outArchiveFilePatterns)
                + ", outDirPatterns: " + patternSetToString(this.outDirPatterns)
                + ", outExtensions: " + stringSetToString(this.outExtensions)
                + ", outFilePatterns: " + patternSetToString(this.outFilePatterns)
                + ", outFileTypes: " + fileTypeSetToString(this.outFileTypes)
                + ", paths: " + stringSetToString(this.paths)
                + ", printUsage: " + this.printUsage
                + ", printVersion: " + this.printVersion
                + ", recursive: " + this.recursive
                + ", sortBy: " + SortByUtil.toName(this.sortBy)
                + ", sortCaseInsensitive: " + this.sortCaseInsensitive
                + ", sortDescending: " + this.sortDescending
                + ", verbose: " + this.verbose
                + ")";
    }
}
