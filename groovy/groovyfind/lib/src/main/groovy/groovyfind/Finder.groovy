package groovyfind

import groovy.transform.CompileStatic

import java.nio.file.DirectoryStream
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.attribute.FileTime
import java.time.Instant
import java.time.ZoneOffset
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CompletionException
import java.util.regex.Pattern

import static groovyfind.FindError.*

@CompileStatic
class Finder {

    final private FindSettings settings
    final private FileTypes fileTypes

    Finder(final FindSettings settings) {
        this.settings = settings
        this.fileTypes = new FileTypes()
    }

    final void validateSettings() throws FindException {
        Set<Path> paths = settings.paths
        if (null == paths || paths.empty || paths.any { p -> p == null || p.empty }) {
            throw new FindException(STARTPATH_NOT_DEFINED.message)
        }
        paths.each { path ->
            if (path == null || path.toString().isEmpty()) {
                throw new FindException(STARTPATH_NOT_DEFINED.message)
            }
            Path p = path
            if (!Files.exists(p)) {
                p = FileUtil.expandPath(p)
                if (!Files.exists(p)) {
                    throw new FindException(STARTPATH_NOT_FOUND.message)
                }
            }
            if (!Files.isReadable(p)) {
                throw new FindException(STARTPATH_NOT_READABLE.message)
            }
            if (Files.isSymbolicLink(p)) {
                if (!settings.followSymlinks) {
                    throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
                }
            } else if (Files.isDirectory(p)) {
                if (!isTraversableDirPath(p)) {
                    throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
                }
            } else if (Files.isRegularFile(p)) {
                if (filterToFileResult(p).isEmpty()) {
                    throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
                }
            } else {
                // TODO: start path is unknown/invalid type
                throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
            }
        }
        if (settings.maxDepth > -1 && settings.maxDepth < settings.minDepth) {
            throw new FindException(INVALID_RANGE_FOR_MINDEPTH_AND_MAXDEPTH.message)
        }
        if (settings.maxLastMod != null && settings.minLastMod != null
                && settings.maxLastMod.toInstant(ZoneOffset.UTC) < settings.minLastMod.toInstant(ZoneOffset.UTC)) {
            throw new FindException(INVALID_RANGE_FOR_MINLASTMOD_AND_MAXLASTMOD.message)
        }
        if (settings.maxSize > 0 && settings.minSize > 0 && settings.maxSize < settings.minSize) {
            throw new FindException(INVALID_RANGE_FOR_MINSIZE_AND_MAXSIZE.message)
        }
    }

    static boolean matchesAnyPattern(final String s, final Set<Pattern> patternSet) {
        patternSet.stream().anyMatch(p -> p.matcher(s).find())
    }

    static boolean anyMatchesAnyPattern(final List<String> sList, final Set<Pattern> patternSet) {
        sList.any { s -> matchesAnyPattern(s, patternSet) }
    }

    static boolean emptyOrMatchesAnyPattern(final String s, final Set<Pattern> patternSet) {
        patternSet.empty || matchesAnyPattern(s, patternSet)
    }

    static boolean emptyOrNotMatchesAnyPattern(final String s, final Set<Pattern> patternSet) {
        patternSet.empty || !matchesAnyPattern(s, patternSet)
    }

    static boolean emptyOrAnyMatchesAnyPattern(final List<String> sList, final Set<Pattern> patternSet) {
        patternSet.empty || anyMatchesAnyPattern(sList, patternSet)
    }

    static boolean emptyOrNotAnyMatchesAnyPattern(final List<String> sList, final Set<Pattern> patternSet) {
        patternSet.empty || !anyMatchesAnyPattern(sList, patternSet)
    }

    static boolean emptyOrMatchesAnyString(final String s, final Set<String> stringSet) {
        stringSet.empty || stringSet.contains(s)
    }

    static boolean emptyOrNotMatchesAnyString(final String s, final Set<String> stringSet) {
        stringSet.empty || !stringSet.contains(s)
    }

    static boolean emptyOrMatchesAnyFileType(final FileType fileType, final Set<FileType> fileTypeSet) {
        fileTypeSet.empty || fileTypeSet.contains(fileType)
    }

    static boolean emptyOrNotMatchesAnyFileType(final FileType fileType, final Set<FileType> fileTypeSet) {
        fileTypeSet.empty || !fileTypeSet.contains(fileType)
    }

    boolean isMatchingPathBySymlink(final Path path) {
        settings.followSymlinks || !Files.isSymbolicLink(path)
    }

    boolean isMatchingDirPathByHidden(final Path dirPath) {
        settings.includeHidden || !FileUtil.isHiddenPath(dirPath)
    }

    boolean isMatchingDirPathByInPatterns(final Path dirPath) {
        List<String> pathElems = FileUtil.splitPath(dirPath)
        emptyOrAnyMatchesAnyPattern(pathElems, settings.inDirPatterns)
    }

    boolean isMatchingDirPathByOutPatterns(final Path dirPath) {
        List<String> pathElems = FileUtil.splitPath(dirPath)
        emptyOrNotAnyMatchesAnyPattern(pathElems, settings.outDirPatterns)
    }

    boolean isTraversableDirPath(final Path dirPath) {
        isMatchingDirPathByHidden(dirPath)
                && isMatchingDirPathByOutPatterns(dirPath)
    }

    boolean isMatchingDirPath(final Path dirPath) {
        isMatchingDirPathByHidden(dirPath)
                && isMatchingDirPathByInPatterns(dirPath)
                && isMatchingDirPathByOutPatterns(dirPath)
    }

    boolean isNullOrMatchingDirPath(final Path dirPath) {
        // null or empty dirPath is a match
        if (null == dirPath || dirPath.toString().isEmpty()) {
            return true
        }
        isMatchingDirPath(dirPath)
    }

    boolean isMatchingFileNameByHidden(final String fileName) {
        settings.includeHidden || !FileUtil.isHiddenName(fileName)
    }

    boolean isMatchingArchiveExtension(final String ext) {
        emptyOrMatchesAnyString(ext, settings.inArchiveExtensions)
                && emptyOrNotMatchesAnyString(ext, settings.outArchiveExtensions)
    }

    boolean isMatchingArchiveExtensionForFilePath(final Path filePath) {
        if (!settings.inArchiveExtensions.empty || !settings.outArchiveExtensions.empty) {
            return isMatchingArchiveExtension(FileUtil.getExtension(filePath.fileName.toString()))
        }
        true
    }

    boolean isMatchingArchiveFileName(final String fileName) {
        return emptyOrMatchesAnyPattern(fileName, settings.inArchiveFilePatterns)
                && emptyOrNotMatchesAnyPattern(fileName, settings.outArchiveFilePatterns)
    }

    boolean isMatchingArchiveFileNameForFilePath(final Path filePath) {
        if (!settings.inArchiveFilePatterns.empty || !settings.outArchiveFilePatterns.empty) {
            var fileName = filePath.getFileName().toString()
            return isMatchingArchiveFileName(fileName)
        }
        true
    }

    boolean isMatchingArchiveFilePath(final Path filePath) {
        isNullOrMatchingDirPath(filePath.parent)
                && isMatchingFileNameByHidden(filePath.fileName.toString())
                && isMatchingArchiveExtensionForFilePath(filePath)
                && isMatchingArchiveFileNameForFilePath(filePath)
    }

    // TODO: add tests
    boolean isMatchingArchiveFileResult(final FileResult fr) {
        return isMatchingArchiveFilePath(fr.path)
    }

    boolean isMatchingExtension(final String ext) {
        emptyOrMatchesAnyString(ext, settings.inExtensions)
                && emptyOrNotMatchesAnyString(ext, settings.outExtensions)
    }

    boolean isMatchingExtensionForFilePath(final Path filePath) {
        if (!settings.inExtensions.empty || !settings.outExtensions.empty) {
            return isMatchingExtension(FileUtil.getExtension(filePath))
        }
        true
    }

    boolean isMatchingFileName(final String fileName) {
        emptyOrMatchesAnyPattern(fileName, settings.inFilePatterns)
                && emptyOrNotMatchesAnyPattern(fileName, settings.outFilePatterns)
    }

    boolean isMatchingFileNameForFilePath(final Path filePath) {
        if (!settings.inFilePatterns.empty || !settings.outFilePatterns.empty) {
            return isMatchingFileName(filePath.fileName.toString())
        }
        true
    }

    boolean isMatchingFilePath(final Path filePath) {
        isNullOrMatchingDirPath(filePath.parent)
                && isMatchingFileNameByHidden(filePath.fileName.toString())
                && isMatchingExtensionForFilePath(filePath)
                && isMatchingFileNameForFilePath(filePath)
    }

    boolean isMatchingFileType(final FileType fileType) {
        emptyOrMatchesAnyFileType(fileType, settings.inFileTypes)
                && emptyOrNotMatchesAnyFileType(fileType, settings.outFileTypes)
    }

    boolean isMatchingFileSize(final long fileSize) {
        ((settings.maxSize <= 0 || fileSize <= settings.maxSize)
                && (settings.minSize <= 0 || fileSize >= settings.minSize))
    }

    boolean isMatchingLastMod(final Instant lastMod) {
        if (settings.maxLastMod == null && settings.minLastMod == null) {
            return true
        }
        if (lastMod == null) {
            return false
        }
        if (settings.maxLastMod != null
                && lastMod > settings.maxLastMod.toInstant(ZoneOffset.UTC)) {
            return false
        }
        settings.minLastMod != null
                || lastMod >= settings.minLastMod.toInstant(ZoneOffset.UTC)
    }

    boolean isMatchingLastMod(final FileTime lastMod) {
        var lastModInstant = lastMod == null ? null : lastMod.toInstant()
        isMatchingLastMod(lastModInstant)
    }

    boolean isMatchingFileResult(final FileResult fr) {
        isMatchingFilePath(fr.path)
                && isMatchingFileType(fr.fileType)
                && isMatchingFileSize(fr.fileSize)
                && isMatchingLastMod(fr.lastMod)
    }

    Optional<FileResult> filterArchiveFilePathToFileResult(final Path filePath, final FileType fileType) {
        if (!settings.includeArchives && !settings.archivesOnly) {
            return Optional.empty()
        }
        if (!isMatchingArchiveFilePath(filePath)) {
            return Optional.empty()
        }
        Optional.of(new FileResult(filePath, fileType, 0L, null))
    }

    Optional<FileResult> filterRegularFilePathToFileResult(final Path filePath, final FileType fileType) {
        if (settings.archivesOnly) {
            return Optional.empty()
        }

        if (!isMatchingFilePath(filePath) || !isMatchingFileType(fileType)) {
            return Optional.empty()
        }

        long fileSize = 0L
        FileTime lastMod = null
        if (settings.needLastMod() || settings.needSize()) {
            try {
                BasicFileAttributes stat = Files.readAttributes(filePath, BasicFileAttributes.class)
                if (settings.needSize()) {
                    fileSize = stat.size()
                }
                if (settings.needLastMod()) {
                    lastMod = stat.lastModifiedTime()
                }
            } catch (IOException e) {
                Logger.logError(e.message)
                return Optional.empty()
            }

            if (!isMatchingFileSize(fileSize) | !isMatchingLastMod(lastMod)) {
                return Optional.empty()
            }
        }

        Optional.of(new FileResult(filePath, fileType, fileSize, lastMod))
    }

    Optional<FileResult> filterToFileResult(final Path filePath) {
        if (!isNullOrMatchingDirPath(filePath.parent)
                || !isMatchingFileNameByHidden(filePath.fileName.toString())) {
            return Optional.empty()
        }

        FileType fileType = fileTypes.getFileType(filePath)
        if (fileType == FileType.ARCHIVE) {
            return filterArchiveFilePathToFileResult(filePath, fileType)
        }
        filterRegularFilePathToFileResult(filePath, fileType)
    }

    private List<FileResult> recFindPath(final Path path, int minDepth, int maxDepth, int currentDepth) {
        List<FileResult> pathResults = new ArrayList<FileResult>()
        boolean recurse = true
        if (currentDepth == maxDepth) {
            recurse = false
        } else if (maxDepth > -1 && currentDepth > maxDepth) {
            return pathResults
        }
        List<Path> pathDirs = new ArrayList<Path>()
        try (DirectoryStream<Path> subPathStream = Files.newDirectoryStream(path)) {
            for (Path subPath : subPathStream) {
                if (!isMatchingPathBySymlink(subPath)) {
                    continue
                }
                if (Files.isDirectory(subPath) && recurse && isTraversableDirPath(subPath)) {
                    pathDirs.add(subPath)
                } else if (Files.isRegularFile(subPath) && (minDepth < 0 || currentDepth >= minDepth)) {
                    filterToFileResult(subPath).ifPresent(pathResults::add)
                }
            }
        } catch (IOException e) {
            e.printStackTrace()
        }
        for (Path pathDir : pathDirs) {
            pathResults.addAll(recFindPath(pathDir, minDepth, maxDepth, currentDepth + 1))
        }
        pathResults
    }

    private List<FileResult> findPath(Path path) throws FindException {
        if (!Files.exists(path)) {
            path = FileUtil.expandPath(path)
            if (!Files.exists(path)) {
                throw new FindException(STARTPATH_NOT_FOUND.message)
            }
        }
        if (Files.isSymbolicLink(path) && !settings.followSymlinks) {
            throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
        }
        if (Files.isDirectory(path)) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (settings.maxDepth == 0) {
                return Collections.emptyList()
            }
            if (isTraversableDirPath(path)) {
                int maxDepth = settings.maxDepth
                if (!settings.recursive) {
                    maxDepth = 1
                }
                return recFindPath(path, settings.minDepth, maxDepth, 1)
            } else {
                throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
            }
        } else if (Files.isRegularFile(path)) {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (settings.minDepth > 0) {
                return Collections.emptyList()
            }
            Optional<FileResult> optFileResult = filterToFileResult(path)
            if (optFileResult.isPresent()) {
                return List.of(optFileResult.get())
            } else {
                throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
            }
        } else {
            throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
        }
    }

    private List<FileResult> findAsync() throws FindException {
        var futures = settings.paths.stream()
                .map(path -> CompletableFuture.supplyAsync({
                    try {
                        return findPath(path)
                    } catch (FindException e) {
                        throw new CompletionException(e)
                    }
                }))
                .toList()

        CompletableFuture.allOf(futures as CompletableFuture[]).join()

        try {
            return futures.stream()
                    .map(CompletableFuture::join)
                    .flatMap(List::stream)
                    .toList() as List<FileResult>
        } catch (CompletionException e) {
            throw new FindException(e.cause.message)
        }
    }

    final List<FileResult> find() throws FindException {
        List<FileResult> fileResults

        if (settings.paths.size() == 1) {
            fileResults = findPath(settings.paths.iterator().next())
        } else {
            fileResults = findAsync()
        }

        if (fileResults.size() > 1) {
            FileResultSorter fileResultSorter = new FileResultSorter(settings)
            fileResultSorter.sort(fileResults)
        }
        fileResults
    }

    private static List<Path> getMatchingDirs(final List<FileResult> results) {
        results.findAll { fr -> fr.path.parent != null }
                .collect { fr -> fr.path.parent }.unique().sort()
    }

    static void printMatchingDirs(final List<FileResult> results, final FileResultFormatter formatter) {
        List<Path> dirs = getMatchingDirs(results)
        if (!dirs.empty) {
            Logger.log("\nMatching directories (${dirs.size()}):")
            dirs.each { Logger.log(formatter.formatDirPath(it)) }
        } else {
            Logger.log('\nMatching directories: 0')
        }
    }

    static void printMatchingFiles(final List<FileResult> results, final FileResultFormatter formatter) {
        if (!results.isEmpty()) {
            Logger.log(String.format("\nMatching files (%d):", results.size()))
            results.each { Logger.log(formatter.formatFileResult(it)) }
        } else {
            Logger.log("\nMatching files: 0")
        }
    }
}
