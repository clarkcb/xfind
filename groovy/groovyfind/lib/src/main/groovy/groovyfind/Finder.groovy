package groovyfind

import groovy.transform.CompileStatic

import java.nio.file.DirectoryStream
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.attribute.FileTime
import java.time.Instant
import java.time.ZoneOffset
import java.util.regex.Pattern

@CompileStatic
class Finder {

    public static final String STARTPATH_NOT_DEFINED = 'Startpath not defined'
    public static final String STARTPATH_NOT_READABLE = 'Startpath not readable'
    public static final String STARTPATH_NOT_FOUND = 'Startpath not found'
    public static final String INVALID_RANGE_FOR_MINDEPTH_AND_MAXDEPTH = 'Invalid range for mindepth and maxdepth'
    public static final String INVALID_RANGE_FOR_MINLASTMOD_AND_MAXLASTMOD = 'Invalid range for minlastmod and maxlastmod'
    public static final String INVALID_RANGE_FOR_MINSIZE_AND_MAXSIZE = 'Invalid range for minsize and maxsize'
    public static final String STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS = 'Startpath does not match find settings'

    final private FindSettings settings
    final private FileTypes fileTypes

    Finder(final FindSettings settings) {
        this.settings = settings
        this.fileTypes = new FileTypes()
    }

    final void validateSettings() throws FindException {
        Set<Path> paths = settings.paths
        if (null == paths || paths.empty || paths.any { p -> p == null || p.empty }) {
            throw new FindException(STARTPATH_NOT_DEFINED)
        }
        paths.each { path ->
            Path p = path
            if (!Files.exists(p)) {
                p = FileUtil.expandPath(p)
            }
            if (Files.exists(p)) {
                if (!Files.isReadable(p)) {
                    throw new FindException(STARTPATH_NOT_READABLE)
                }
            } else {
                throw new FindException(STARTPATH_NOT_FOUND)
            }
        }
        if (settings.maxDepth > -1 && settings.maxDepth < settings.minDepth) {
            throw new FindException(INVALID_RANGE_FOR_MINDEPTH_AND_MAXDEPTH)
        }
        if (settings.maxLastMod != null && settings.minLastMod != null
                && settings.maxLastMod.toInstant(ZoneOffset.UTC) < settings.minLastMod.toInstant(ZoneOffset.UTC)) {
            throw new FindException(INVALID_RANGE_FOR_MINLASTMOD_AND_MAXLASTMOD)
        }
        if (settings.maxSize > 0 && settings.minSize > 0 && settings.maxSize < settings.minSize) {
            throw new FindException(INVALID_RANGE_FOR_MINSIZE_AND_MAXSIZE)
        }
    }

    private static boolean anyMatchesAnyPattern(final List<String> sList,
                                                final Set<Pattern> patternSet) {
        sList.any { s -> matchesAnyPattern(s, patternSet) }
    }

    private static boolean matchesAnyPattern(final String s,
                                             final Set<Pattern> patternSet) {
        null != s && patternSet.stream().anyMatch(p -> p.matcher(s).find())
    }

    boolean isMatchingDir(final Path path) {
        // null or empty path is a match
        if (null == path || path.toString().isEmpty()) {
            return true
        }
        if (!settings.includeHidden) {
            try {
                // This erroneously returns true for . and ..
//                if (Files.isHidden(path)) {
//                    return false;
//                }
                if (FileUtil.isHidden(path)) {
                    return false
                }
            } catch (Exception e) {
                Logger.logError(e.message)
                return false
            }
        }
        List<String> pathElems = FileUtil.splitPath(path)
        (settings.inDirPatterns.empty
                ||
                anyMatchesAnyPattern(pathElems, settings.inDirPatterns))
                &&
                (settings.outDirPatterns.empty
                        ||
                        !anyMatchesAnyPattern(pathElems, settings.outDirPatterns))
    }

    boolean isMatchingExtension(final String ext) {
        ((settings.inExtensions.empty
                || ext in settings.inExtensions)
                &&
                (settings.outExtensions.isEmpty()
                        || !settings.outExtensions.contains(ext)))
    }

    boolean hasMatchingExtension(final FileResult fr) {
        if (!settings.inExtensions.empty || !settings.outExtensions.empty) {
            String fileName = fr.getPath().getFileName().toString()
            String ext = FileUtil.getExtension(fileName)
            return isMatchingExtension(ext)
        }
        true
    }

    boolean isMatchingFileName(final String fileName) {
        ((settings.inFilePatterns.empty
                || matchesAnyPattern(fileName, settings.inFilePatterns))
                &&
                (settings.outFilePatterns.empty
                        || !matchesAnyPattern(fileName, settings.outFilePatterns)))
    }

    boolean isMatchingFileType(final FileType fileType) {
        ((settings.inFileTypes.empty
                || fileType in settings.inFileTypes)
                &&
                (settings.outFileTypes.empty
                        || !settings.outFileTypes.contains(fileType)))
    }

    boolean isMatchingFileSize(final long fileSize) {
        ((settings.maxSize <= 0 || fileSize <= settings.maxSize)
                &&
                (settings.minSize <= 0 || fileSize >= settings.minSize))
    }

    boolean isMatchingLastMod(final Instant lastMod) {
        ((settings.maxLastMod == null
                || lastMod <= settings.maxLastMod.toInstant(ZoneOffset.UTC)
                &&
                (settings.minLastMod == null
                        || lastMod >= settings.minLastMod.toInstant(ZoneOffset.UTC))))
    }

    boolean isMatchingFileResult(final FileResult fr) {
        hasMatchingExtension(fr)
                && isMatchingFileName(fr.path.fileName.toString())
                && isMatchingFileType(fr.fileType)
                && isMatchingFileSize(fr.fileSize)
                && isMatchingLastMod(fr.lastMod == null ? null : fr.lastMod.toInstant())
    }

    boolean isMatchingArchiveFile(final Path path) {
        String fileName = path.fileName.toString()
        if (!settings.inArchiveExtensions.empty || !settings.outArchiveExtensions.empty) {
            String ext = FileUtil.getExtension(fileName)
            if ((!settings.inArchiveExtensions.empty && !settings.inArchiveExtensions.contains(ext))
                    ||
                    (!settings.outArchiveExtensions.empty
                            &&
                            settings.outArchiveExtensions.contains(ext))) {
                return false
            }
        }
        (settings.inArchiveFilePatterns.empty
                ||
                matchesAnyPattern(fileName, settings.inArchiveFilePatterns))
                &&
                (settings.outArchiveFilePatterns.empty
                        ||
                        !matchesAnyPattern(fileName, settings.outArchiveFilePatterns))
    }

    Optional<FileResult> filterToFileResult(final Path path) {
        if (!settings.includeHidden) {
            try {
                if (Files.isHidden(path)) {
                    return Optional.empty()
                }
            } catch (IOException e) {
                Logger.logError(e.message)
                return Optional.empty()
            }
        }

        FileType fileType = fileTypes.getFileType(path)
        if (fileType == FileType.ARCHIVE && !settings.includeArchives && !settings.archivesOnly) {
            return Optional.empty()
        }

        long fileSize = 0L
        FileTime lastMod = null
        if (settings.needSize() || settings.needLastMod()) {
            try {
                BasicFileAttributes stat = Files.readAttributes(path, BasicFileAttributes.class)
                fileSize = stat.size()
                lastMod = stat.lastModifiedTime()
            } catch (IOException e) {
                Logger.logError(e.message)
                return Optional.empty()
            }
        }

        FileResult fileResult = new FileResult(path, fileType, fileSize, lastMod)
        if (fileResult.fileType == FileType.ARCHIVE) {
            if (isMatchingArchiveFile(path)) {
                return Optional.of(fileResult)
            }
            return Optional.empty()
        }
        if (!settings.archivesOnly && isMatchingFileResult(fileResult)) {
            return Optional.of(fileResult)
        }
        Optional.empty()
    }

    final Comparator<FileResult> getFileResultComparator() {
        if (settings.sortDescending) {
            switch (settings.sortBy) {
                case SortBy.FILENAME:
                    return (FileResult fr1, FileResult fr2) -> fr2.compareByName(fr1, settings.sortCaseInsensitive)
                case SortBy.FILESIZE:
                    return (FileResult fr1, FileResult fr2) -> fr2.compareBySize(fr1, settings.sortCaseInsensitive)
                case SortBy.FILETYPE:
                    return (FileResult fr1, FileResult fr2) -> fr2.compareByType(fr1, settings.sortCaseInsensitive)
                case SortBy.LASTMOD:
                    return (FileResult fr1, FileResult fr2) -> fr2.compareByLastMod(fr1, settings.sortCaseInsensitive)
                default:
                    return (FileResult fr1, FileResult fr2) -> fr2.compareByPath(fr1, settings.sortCaseInsensitive)
            }
        }
        switch (settings.sortBy) {
            case SortBy.FILENAME:
                return (FileResult fr1, FileResult fr2) -> fr1.compareByName(fr2, settings.sortCaseInsensitive)
            case SortBy.FILESIZE:
                return (FileResult fr1, FileResult fr2) -> fr1.compareBySize(fr2, settings.sortCaseInsensitive)
            case SortBy.FILETYPE:
                return (FileResult fr1, FileResult fr2) -> fr1.compareByType(fr2, settings.sortCaseInsensitive)
            case SortBy.LASTMOD:
                return (FileResult fr1, FileResult fr2) -> fr1.compareByLastMod(fr2, settings.sortCaseInsensitive)
            default:
                return (FileResult fr1, FileResult fr2) -> fr1.compareByPath(fr2, settings.sortCaseInsensitive)
        }
    }

    final void sortFileResults(List<FileResult> fileResults) {
        if (fileResults.empty) {
            return
        }
        def comparator = getFileResultComparator()
        fileResults.sort(comparator)
    }

    private List<FileResult> recFindPath(final Path filePath, int minDepth, int maxDepth, int currentDepth) {
        List<FileResult> pathResults = new ArrayList<FileResult>()
        boolean recurse = true
        if (currentDepth == maxDepth) {
            recurse = false
        } else if (maxDepth > -1 && currentDepth > maxDepth) {
            return pathResults
        }
        List<Path> pathDirs = new ArrayList<Path>()
        try (DirectoryStream<Path> pathContents = Files.newDirectoryStream(filePath)) {
            for (Path path : pathContents) {
                if (Files.isSymbolicLink(path) && !settings.followSymlinks) {
                    continue
                }
                if (Files.isDirectory(path) && recurse && isMatchingDir(path)) {
                    pathDirs.add(path)
                } else if (Files.isRegularFile(path) && (minDepth < 0 || currentDepth >= minDepth)) {
                    Optional<FileResult> optFileResult = filterToFileResult(path)
                    optFileResult.ifPresent(pathResults::add)
                }
            }
            for (Path pathDir : pathDirs) {
                pathResults.addAll(recFindPath(pathDir, minDepth, maxDepth, currentDepth + 1))
            }
        } catch (IOException e) {
            e.printStackTrace()
        }
        pathResults
    }

    private List<FileResult> findPath(Path filePath) throws FindException {
        if (!Files.exists(filePath)) {
            filePath = FileUtil.expandPath(filePath)
        }
        if (Files.isDirectory(filePath)) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (settings.maxDepth == 0) {
                return Collections.emptyList()
            }
            if (isMatchingDir(filePath)) {
                int maxDepth = settings.maxDepth
                if (!settings.getRecursive()) {
                    maxDepth = 1
                }
                return recFindPath(filePath, settings.minDepth, maxDepth, 1)
            } else {
                throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS)
            }
        } else {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (settings.minDepth > 0) {
                return Collections.emptyList()
            }
            Optional<FileResult> optFileResult = filterToFileResult(filePath)
            if (optFileResult.isPresent()) {
                return List.of(optFileResult.get())
            } else {
                throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS)
            }
        }
    }

    final List<FileResult> find() throws FindException {
        List<FileResult> fileResults = []

        // The Futures way - not working, "ERROR: null"
//        ExecutorService executorService
//        if (settings.getPaths().size() == 1) {
//            executorService = Executors.newSingleThreadExecutor()
//        } else {
//            executorService = Executors.newFixedThreadPool(settings.getPaths().size())
//        }
//        List<Future<List<FileResult>>> futures = new ArrayList<>()

        settings.paths.each { path ->
            fileResults.addAll(findPath(path))
//            futures.add(executorService.submit{findPath(path)} as Future<List<FileResult>>)
        }

//        futures.each { future ->
//            try {
//                fileResults.addAll(future.get())
//            } catch (Exception e) {
//                Logger.logError(e.message)
//            }
//        }
//
//        executorService.shutdown()

        sortFileResults(fileResults)
        fileResults
    }
}
