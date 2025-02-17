package groovyfind

import groovy.transform.CompileStatic

import java.nio.file.Path
import java.nio.file.Paths
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import java.util.regex.Pattern

@CompileStatic
class DefaultFindSettings {
    static final boolean ARCHIVES_ONLY = false
    static final boolean DEBUG = false
    static final boolean FOLLOW_SYMLINKS = false
    static final boolean INCLUDE_ARCHIVES = false
    static final boolean INCLUDE_HIDDEN = false
    static final int MAX_DEPTH = -1
    static final long MAX_SIZE = 0L
    static final int MIN_DEPTH = -1
    static final long MIN_SIZE = 0L
    static final boolean PRINT_DIRS = false
    static final boolean PRINT_FILES = true
    static final boolean PRINT_USAGE = false
    static final boolean PRINT_VERSION = false
    static final boolean RECURSIVE = true
    static final SortBy SORT_BY = SortBy.FILEPATH
    static final boolean SORT_CASE_INSENSITIVE = false
    static final boolean SORT_DESCENDING = false
    static final boolean VERBOSE = false
}

@CompileStatic
class FindSettings {

    static final int INITIAL_SET_CAPACITY = 4

    boolean archivesOnly
    boolean debug
    boolean followSymlinks
    final Set<String> inArchiveExtensions
    final Set<Pattern> inArchiveFilePatterns
    final Set<Pattern> inDirPatterns
    final Set<String> inExtensions
    final Set<Pattern> inFilePatterns
    final Set<FileType> inFileTypes
    boolean includeArchives
    boolean includeHidden
    int maxDepth
    LocalDateTime maxLastMod
    long maxSize
    int minDepth
    LocalDateTime minLastMod
    long minSize
    final Set<String> outArchiveExtensions
    final Set<Pattern> outArchiveFilePatterns
    final Set<Pattern> outDirPatterns
    final Set<String> outExtensions
    final Set<Pattern> outFilePatterns
    final Set<FileType> outFileTypes
    Set<Path> paths
    boolean printDirs
    boolean printFiles
    boolean printUsage
    boolean printVersion
    boolean recursive
    SortBy sortBy
    boolean sortCaseInsensitive
    boolean sortDescending
    boolean verbose

    FindSettings() {
        this.archivesOnly = DefaultFindSettings.ARCHIVES_ONLY
        this.debug = DefaultFindSettings.DEBUG
        this.followSymlinks = DefaultFindSettings.FOLLOW_SYMLINKS
        this.inArchiveExtensions = new LinkedHashSet<>(INITIAL_SET_CAPACITY)
        this.inArchiveFilePatterns = new LinkedHashSet<>(INITIAL_SET_CAPACITY)
        this.inDirPatterns = new LinkedHashSet<>(INITIAL_SET_CAPACITY)
        this.inExtensions = new LinkedHashSet<>(INITIAL_SET_CAPACITY)
        this.inFilePatterns = new LinkedHashSet<>(INITIAL_SET_CAPACITY)
        this.inFileTypes = new LinkedHashSet<>(INITIAL_SET_CAPACITY)
        this.includeArchives = DefaultFindSettings.INCLUDE_ARCHIVES
        this.includeHidden = DefaultFindSettings.INCLUDE_HIDDEN
        this.maxDepth = DefaultFindSettings.MAX_DEPTH
        this.maxLastMod = null
        this.maxSize = DefaultFindSettings.MAX_SIZE
        this.minDepth = DefaultFindSettings.MIN_DEPTH
        this.minLastMod = null
        this.minSize = DefaultFindSettings.MIN_SIZE
        this.outArchiveExtensions = new LinkedHashSet<>(INITIAL_SET_CAPACITY)
        this.outArchiveFilePatterns = new LinkedHashSet<>(INITIAL_SET_CAPACITY)
        this.outDirPatterns = new LinkedHashSet<>(INITIAL_SET_CAPACITY)
        this.outExtensions = new LinkedHashSet<>(INITIAL_SET_CAPACITY)
        this.outFilePatterns = new LinkedHashSet<>(INITIAL_SET_CAPACITY)
        this.outFileTypes = new LinkedHashSet<>(INITIAL_SET_CAPACITY)
        this.paths = new LinkedHashSet<>(INITIAL_SET_CAPACITY)
        this.printDirs = DefaultFindSettings.PRINT_DIRS
        this.printFiles = DefaultFindSettings.PRINT_FILES
        this.printUsage = DefaultFindSettings.PRINT_USAGE
        this.printVersion = DefaultFindSettings.PRINT_VERSION
        this.recursive = DefaultFindSettings.RECURSIVE
        this.sortBy = DefaultFindSettings.SORT_BY
        this.sortCaseInsensitive = DefaultFindSettings.SORT_CASE_INSENSITIVE
        this.sortDescending = DefaultFindSettings.SORT_DESCENDING
        this.verbose = DefaultFindSettings.VERBOSE
    }

    final void addPath(final String path) {
        this.paths.add(Paths.get(path))
    }

    void setArchivesOnly(final boolean archivesOnly) {
        this.archivesOnly = archivesOnly
        if (archivesOnly) {
            this.includeArchives = true
        }
    }

    final void setDebug(final boolean debug) {
        this.debug = debug
        if (debug) {
            this.verbose = true
        }
    }

    static LocalDateTime getLastModFromString(final String lastModString) {
        LocalDateTime lastMod = null
        try {
            lastMod = LocalDateTime.parse(lastModString)
        } catch (DateTimeParseException e) {
            try {
                LocalDate maxLastModDate = LocalDate.parse(lastModString, DateTimeFormatter.ISO_LOCAL_DATE)
                lastMod = maxLastModDate.atTime(0, 0, 0)
            } catch (DateTimeParseException e2) {
                System.out.println('Unable to parse lastModString')
            }
        }
        lastMod
    }

    void setMaxLastModFromString(final String maxLastModString) {
        this.maxLastMod = getLastModFromString(maxLastModString)
    }

    void setMinLastModFromString(final String minLastModString) {
        this.minLastMod = getLastModFromString(minLastModString)
    }

    // could be a comma-separated list
    static void addExtensions(Set<String> set, final String exts) {
        addExtensions(set, Arrays.asList(exts.split(',')))
    }

    static void addExtensions(Set<String> set, final List<String> exts) {
        exts.each { x ->
            if (!x.isEmpty()) {
                set.add(x.toLowerCase())
            }
        }
    }

    final void addInExtension(final String ext) {
        addExtensions(this.inExtensions, ext)
    }

    final void addOutExtension(final String ext) {
        addExtensions(this.outExtensions, ext)
    }

    final void addInArchiveExtension(final String ext) {
        addExtensions(this.inArchiveExtensions, ext)
    }

    final void addOutArchiveExtension(final String ext) {
        addExtensions(this.outArchiveExtensions, ext)
    }

    private static void addPattern(Set<Pattern> set, final String pattern) {
        set.add(Pattern.compile(pattern))
    }

    final void addInDirPattern(final String pattern) {
        addPattern(this.inDirPatterns, pattern)
    }

    final void addOutDirPattern(final String pattern) {
        addPattern(this.outDirPatterns, pattern)
    }

    final void addInFilePattern(final String pattern) {
        addPattern(this.inFilePatterns, pattern)
    }

    final void addOutFilePattern(final String pattern) {
        addPattern(this.outFilePatterns, pattern)
    }

    final void addInArchiveFilePattern(final String pattern) {
        addPattern(this.inArchiveFilePatterns, pattern)
    }

    final void addOutArchiveFilePattern(final String pattern) {
        addPattern(this.outArchiveFilePatterns, pattern)
    }

    // could be a comma-separated list
    static void addFileTypes(Set<FileType> set, final String fts) {
        addFileTypes(set, Arrays.asList(fts.split(',')))
    }

    static void addFileTypes(Set<FileType> set, final List<String> fts) {
        fts.each { ft ->
            if (!ft.isEmpty()) {
                set.add(FileType.forName(ft))
            }
        }
    }

    final void addInFileType(final String ft) {
        addFileTypes(this.inFileTypes, ft)
    }

    final void addOutFileType(final String ft) {
        addFileTypes(this.outFileTypes, ft)
    }

    boolean needLastMod() {
        this.sortBy == SortBy.LASTMOD ||
                this.maxLastMod != null || this.minLastMod != null
    }

    boolean needSize() {
        this.sortBy == SortBy.FILESIZE ||
                this.maxSize > 0 || this.minSize > 0
    }

    static String setToString(final Set<String> set, final boolean quote) {
        StringBuilder sb = new StringBuilder('[')
        int elemCount = 0
        set.each { s ->
            if (elemCount > 0) {
                sb.append(', ')
            }
            if (quote) {
                sb.append('"').append(s).append('"')
            } else {
                sb.append(s)
            }
            elemCount++
        }
        sb.append(']')
        sb.toString()
    }

    static String stringSetToString(final Set<String> set) {
        setToString(set, true)
    }

    static String pathSetToString(final Set<Path> set) {
        Set<String> stringSet = set.collect { p -> p.toString() }.toSet()
        setToString(stringSet, true)
    }

    static String patternSetToString(final Set<Pattern> set) {
        Set<String> stringSet = set.collect { p -> p.toString() }.toSet()
        setToString(stringSet, true)
    }

    static String fileTypeSetToString(final Set<FileType> set) {
        Set<String> stringSet = set.collect { ft -> ft.toName() }.toSet()
        setToString(stringSet, false)
    }

    static String localDateTimeToString(final LocalDateTime dt) {
        if (dt == null) {
            '0'
        } else {
            "\"${dt}\""
        }
    }

    String toString() {
        'FindSettings(' +
                'archivesOnly=' + this.archivesOnly +
                ', debug=' + this.debug +
                ', followSymlinks=' + this.followSymlinks +
                ', inArchiveExtensions=' + stringSetToString(this.inArchiveExtensions) +
                ', inArchiveFilePatterns=' + patternSetToString(this.inArchiveFilePatterns) +
                ', inDirPatterns=' + patternSetToString(this.inDirPatterns) +
                ', inExtensions=' + stringSetToString(this.inExtensions) +
                ', inFilePatterns=' + patternSetToString(this.inFilePatterns) +
                ', inFileTypes=' + fileTypeSetToString(this.inFileTypes) +
                ', includeArchives=' + this.includeArchives +
                ', includeHidden=' + this.includeHidden +
                ', maxDepth=' + this.maxDepth +
                ', maxLastMod=' + localDateTimeToString(this.maxLastMod) +
                ', maxSize=' + this.maxSize +
                ', minDepth=' + this.minDepth +
                ', minLastMod=' + localDateTimeToString(this.minLastMod) +
                ', minSize=' + this.minSize +
                ', outArchiveExtensions=' + stringSetToString(this.outArchiveExtensions) +
                ', outArchiveFilePatterns=' + patternSetToString(this.outArchiveFilePatterns) +
                ', outDirPatterns=' + patternSetToString(this.outDirPatterns) +
                ', outExtensions=' + stringSetToString(this.outExtensions) +
                ', outFilePatterns=' + patternSetToString(this.outFilePatterns) +
                ', outFileTypes=' + fileTypeSetToString(this.outFileTypes) +
                ', paths=' + pathSetToString(this.paths) +
                ', printDirs=' + this.printDirs +
                ', printFiles=' + this.printFiles +
                ', printUsage=' + this.printUsage +
                ', printVersion=' + this.printVersion +
                ', recursive=' + this.recursive +
                ', sortBy=' + this.sortBy.toName() +
                ', sortCaseInsensitive=' + this.sortCaseInsensitive +
                ', sortDescending=' + this.sortDescending +
                ', verbose=' + this.verbose +
                ')'
    }
}
