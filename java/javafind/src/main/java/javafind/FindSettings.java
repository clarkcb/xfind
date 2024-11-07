/*******************************************************************************
FindSettings

Class to encapsulate find settings

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

public class FindSettings {

    private static final int INITIAL_SET_CAPACITY = 4;

    private boolean archivesOnly;
    private boolean debug;
    private boolean followSymlinks;
    private final Set<String> inArchiveExtensions;
    private final Set<Pattern> inArchiveFilePatterns;
    private final Set<Pattern> inDirPatterns;
    private final Set<String> inExtensions;
    private final Set<Pattern> inFilePatterns;
    private final Set<FileType> inFileTypes;
    private boolean includeArchives;
    private boolean includeHidden;
    private int maxDepth;
    private LocalDateTime maxLastMod;
    private long maxSize;
    private int minDepth;
    private LocalDateTime minLastMod;
    private long minSize;
    private final Set<String> outArchiveExtensions;
    private final Set<Pattern> outArchiveFilePatterns;
    private final Set<Pattern> outDirPatterns;
    private final Set<String> outExtensions;
    private final Set<Pattern> outFilePatterns;
    private final Set<FileType> outFileTypes;
    private Set<Path> paths;
    private boolean printDirs;
    private boolean printFiles;
    private boolean printUsage;
    private boolean printVersion;
    private boolean recursive;
    private SortBy sortBy;
    private boolean sortCaseInsensitive;
    private boolean sortDescending;
    private boolean verbose;

    public FindSettings() {
        this.archivesOnly = DefaultFindSettings.ARCHIVES_ONLY;
        this.debug = DefaultFindSettings.DEBUG;
        this.followSymlinks = DefaultFindSettings.FOLLOW_SYMLINKS;
        this.inArchiveExtensions = new LinkedHashSet<>(INITIAL_SET_CAPACITY);
        this.inArchiveFilePatterns = new LinkedHashSet<>(INITIAL_SET_CAPACITY);
        this.inDirPatterns = new LinkedHashSet<>(INITIAL_SET_CAPACITY);
        this.inExtensions = new LinkedHashSet<>(INITIAL_SET_CAPACITY);
        this.inFilePatterns = new LinkedHashSet<>(INITIAL_SET_CAPACITY);
        this.inFileTypes = new LinkedHashSet<>(INITIAL_SET_CAPACITY);
        this.includeArchives = DefaultFindSettings.INCLUDE_ARCHIVES;
        this.includeHidden = DefaultFindSettings.INCLUDE_HIDDEN;
        this.maxDepth = DefaultFindSettings.MAX_DEPTH;
        this.maxLastMod = null;
        this.maxSize = DefaultFindSettings.MAX_SIZE;
        this.minDepth = DefaultFindSettings.MIN_DEPTH;
        this.minLastMod = null;
        this.minSize = DefaultFindSettings.MIN_SIZE;
        this.outArchiveExtensions = new LinkedHashSet<>(INITIAL_SET_CAPACITY);
        this.outArchiveFilePatterns = new LinkedHashSet<>(INITIAL_SET_CAPACITY);
        this.outDirPatterns = new LinkedHashSet<>(INITIAL_SET_CAPACITY);
        this.outExtensions = new LinkedHashSet<>(INITIAL_SET_CAPACITY);
        this.outFilePatterns = new LinkedHashSet<>(INITIAL_SET_CAPACITY);
        this.outFileTypes = new LinkedHashSet<>(INITIAL_SET_CAPACITY);
        this.paths = new LinkedHashSet<>(INITIAL_SET_CAPACITY);
        this.printDirs = DefaultFindSettings.PRINT_DIRS;
        this.printFiles = DefaultFindSettings.PRINT_FILES;
        this.printUsage = DefaultFindSettings.PRINT_USAGE;
        this.printVersion = DefaultFindSettings.PRINT_VERSION;
        this.recursive = DefaultFindSettings.RECURSIVE;
        this.sortBy = DefaultFindSettings.SORT_BY;
        this.sortCaseInsensitive = DefaultFindSettings.SORT_CASE_INSENSITIVE;
        this.sortDescending = DefaultFindSettings.SORT_DESCENDING;
        this.verbose = DefaultFindSettings.VERBOSE;
    }

    public final Set<Path> getPaths() {
        return this.paths;
    }

    public final void setPaths(final Set<Path> paths) {
        this.paths = paths;
    }

    public final void addPath(final Path path) {
        this.paths.add(path);
    }

    public final void addPath(final String path) {
        this.paths.add(Paths.get(path));
    }

    public final boolean getArchivesOnly() {
        return this.archivesOnly;
    }

    public void setArchivesOnly(final boolean archivesOnly) {
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

    public boolean getFollowSymlinks() {
        return followSymlinks;
    }

    public void setFollowSymlinks(boolean followSymlinks) {
        this.followSymlinks = followSymlinks;
    }

    public final boolean getIncludeHidden() {
        return this.includeHidden;
    }

    public final void setIncludeHidden(final boolean includeHidden) {
        this.includeHidden = includeHidden;
    }

    public final boolean getIncludeArchives() {
        return this.includeArchives;
    }

    public final void setIncludeArchives(final boolean includeArchives) {
        this.includeArchives = includeArchives;
    }

    public int getMaxDepth() {
        return maxDepth;
    }

    public void setMaxDepth(final int maxDepth) {
        this.maxDepth = maxDepth;
    }

    public LocalDateTime getMaxLastMod() {
        return maxLastMod;
    }

    private LocalDateTime getLastModFromString(final String lastModString) {
        LocalDateTime lastMod = null;
        try {
            lastMod = LocalDateTime.parse(lastModString);
        } catch (DateTimeParseException e) {
            try {
                var maxLastModDate = LocalDate.parse(lastModString, DateTimeFormatter.ISO_LOCAL_DATE);
                lastMod = maxLastModDate.atTime(0, 0, 0);
            } catch (DateTimeParseException e2) {
                System.out.println("Unable to parse lastModString");
            }
        }
        return lastMod;
    }

    public void setMaxLastMod(final String maxLastModString) {
        this.maxLastMod = getLastModFromString(maxLastModString);
    }

    public void setMaxLastMod(final LocalDateTime maxLastMod) {
        this.maxLastMod = maxLastMod;
    }

    public long getMaxSize() {
        return maxSize;
    }

    public void setMaxSize(final long maxSize) {
        this.maxSize = maxSize;
    }

    public int getMinDepth() {
        return minDepth;
    }

    public void setMinDepth(final int minDepth) {
        this.minDepth = minDepth;
    }

    public LocalDateTime getMinLastMod() {
        return minLastMod;
    }

    public void setMinLastMod(final String minLastModString) {
        this.minLastMod = getLastModFromString(minLastModString);
    }

    public void setMinLastMod(final LocalDateTime minLastMod) {
        this.minLastMod = minLastMod;
    }

    public long getMinSize() {
        return minSize;
    }

    public void setMinSize(final long minSize) {
        this.minSize = minSize;
    }

    public final boolean getPrintDirs() {
        return this.printDirs;
    }

    public final void setPrintDirs(final boolean printDirs) {
        this.printDirs = printDirs;
    }

    public final boolean getPrintFiles() {
        return this.printFiles;
    }

    public final void setPrintFiles(final boolean printFiles) {
        this.printFiles = printFiles;
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
        for (var x : exts) {
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
        for (var ft : fts) {
            if (!ft.isEmpty()) {
                set.add(FileType.forName(ft));
            }
        }
    }

    public final void addInFileType(final String ft) {
        addFileTypes(this.inFileTypes, ft);
    }

    public final void addOutFileType(final String ft) {
        addFileTypes(this.outFileTypes, ft);
    }

    public boolean needLastMod() {
        return this.sortBy.equals(SortBy.LASTMOD) ||
                this.maxLastMod != null || this.minLastMod != null;
    }

    public boolean needSize() {
        return this.sortBy.equals(SortBy.FILESIZE) ||
                this.maxSize > 0 || this.minSize > 0;
    }

    protected static String setToString(final Set<String> set, boolean quotes) {
        var sb = new StringBuilder("[");
        int elemCount = 0;
        for (var s : set) {
            if (elemCount > 0) {
                sb.append(", ");
            }
            if (quotes) {
                sb.append("\"").append(s).append("\"");
            } else {
                sb.append(s);
            }
            elemCount++;
        }
        sb.append("]");
        return sb.toString();
    }

    protected static String stringSetToString(final Set<String> set) {
        return setToString(set, true);
    }

    protected static String pathSetToString(final Set<Path> set) {
        var stringSet = set.stream().map(Path::toString).collect(java.util.stream.Collectors.toSet());
        return setToString(stringSet, true);
    }

    protected static String patternSetToString(final Set<Pattern> set) {
        var stringSet = set.stream().map(Pattern::toString).collect(java.util.stream.Collectors.toSet());
        return setToString(stringSet, true);
    }

    protected static String fileTypeSetToString(final Set<FileType> set) {
        var stringSet = set.stream().map(FileType::toName).collect(java.util.stream.Collectors.toSet());
        return setToString(stringSet, false);
    }

    protected static String localDateTimeToString(final LocalDateTime dt) {
        if (dt == null) {
            return "0";
        }
        return String.format("\"%s\"", dt);
    }

    public String toString() {
        return "FindSettings("
                + "archivesOnly=" + this.archivesOnly
                + ", debug=" + this.debug
                + ", followSymlinks=" + this.followSymlinks
                + ", inArchiveExtensions=" + stringSetToString(this.inArchiveExtensions)
                + ", inArchiveFilePatterns=" + patternSetToString(this.inArchiveFilePatterns)
                + ", inDirPatterns=" + patternSetToString(this.inDirPatterns)
                + ", inExtensions=" + stringSetToString(this.inExtensions)
                + ", inFilePatterns=" + patternSetToString(this.inFilePatterns)
                + ", inFileTypes=" + fileTypeSetToString(this.inFileTypes)
                + ", includeArchives=" + this.includeArchives
                + ", includeHidden=" + this.includeHidden
                + ", maxDepth=" + this.maxDepth
                + ", maxLastMod=" + localDateTimeToString(this.maxLastMod)
                + ", maxSize=" + this.maxSize
                + ", minDepth=" + this.minDepth
                + ", minLastMod=" + localDateTimeToString(this.minLastMod)
                + ", minSize=" + this.minSize
                + ", outArchiveExtensions=" + stringSetToString(this.outArchiveExtensions)
                + ", outArchiveFilePatterns=" + patternSetToString(this.outArchiveFilePatterns)
                + ", outDirPatterns=" + patternSetToString(this.outDirPatterns)
                + ", outExtensions=" + stringSetToString(this.outExtensions)
                + ", outFilePatterns=" + patternSetToString(this.outFilePatterns)
                + ", outFileTypes=" + fileTypeSetToString(this.outFileTypes)
                + ", paths=" + pathSetToString(this.paths)
                + ", printDirs=" + this.printDirs
                + ", printFiles=" + this.printFiles
                + ", printUsage=" + this.printUsage
                + ", printVersion=" + this.printVersion
                + ", recursive=" + this.recursive
                + ", sortBy=" + this.sortBy.toName()
                + ", sortCaseInsensitive=" + this.sortCaseInsensitive
                + ", sortDescending=" + this.sortDescending
                + ", verbose=" + this.verbose
                + ")";
    }
}
