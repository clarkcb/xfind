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

    final private FindSettings settings
    final private FileTypes fileTypes

    Finder(final FindSettings settings) {
        this.settings = settings
        this.fileTypes = new FileTypes()
    }

    final void validateSettings() throws FindException {
        def paths = settings.paths
        if (null == paths || paths.empty || paths.any { p -> p == null || p.empty }) {
            throw new FindException('Startpath not defined')
        }
        paths.each { path ->
            if (!Files.exists(path)) {
                throw new FindException('Startpath not found')
            }
            if (!Files.isReadable(path)) {
                throw new FindException('Startpath not readable')
            }
        }
        if (settings.maxDepth > -1 && settings.maxDepth < settings.minDepth) {
            throw new FindException('Invalid range for mindepth and maxdepth')
        }
        if (settings.maxLastMod != null && settings.minLastMod != null
                && settings.maxLastMod.toInstant(ZoneOffset.UTC) < settings.minLastMod.toInstant(ZoneOffset.UTC)) {
            throw new FindException('Invalid range for minlastmod and maxlastmod')
        }
        if (settings.maxSize > 0 && settings.minSize > 0 && settings.maxSize < settings.minSize) {
            throw new FindException('Invalid range for minsize and maxsize')
        }
    }

    private static boolean anyMatchesAnyPattern(final List<String> sList,
                                                final Set<Pattern> patternSet) {
        return sList.any { s -> matchesAnyPattern(s, patternSet) }
    }

    private static boolean matchesAnyPattern(final String s,
                                             final Set<Pattern> patternSet) {
        return null != s && patternSet.stream().anyMatch(p -> p.matcher(s).find())
    }

    boolean isMatchingDir(final Path path) {
        // null or empty path is a match
        if (null == path || path.toString().isEmpty()) {
            return true;
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
        return (settings.inDirPatterns.empty
                ||
                anyMatchesAnyPattern(pathElems, settings.inDirPatterns))
                &&
                (settings.outDirPatterns.empty
                        ||
                        !anyMatchesAnyPattern(pathElems, settings.outDirPatterns))
    }

    boolean isMatchingExtension(final String ext) {
        return ((settings.getInExtensions().isEmpty()
                || settings.getInExtensions().contains(ext))
                &&
                (settings.getOutExtensions().isEmpty()
                        || !settings.getOutExtensions().contains(ext)))
    }

    boolean hasMatchingExtension(final FileResult fr) {
        if (!settings.getInExtensions().isEmpty() || !settings.getOutExtensions().isEmpty()) {
            def fileName = fr.getPath().getFileName().toString()
            def ext = FileUtil.getExtension(fileName)
            return isMatchingExtension(ext)
        }
        return true
    }

    boolean isMatchingFileName(final String fileName) {
        return ((settings.getInFilePatterns().isEmpty()
                || matchesAnyPattern(fileName, settings.getInFilePatterns()))
                &&
                (settings.getOutFilePatterns().isEmpty()
                        || !matchesAnyPattern(fileName, settings.getOutFilePatterns())))
    }

    boolean isMatchingFileType(final FileType fileType) {
        return ((settings.getInFileTypes().isEmpty()
                || settings.getInFileTypes().contains(fileType))
                &&
                (settings.getOutFileTypes().isEmpty()
                        || !settings.getOutFileTypes().contains(fileType)))
    }

    boolean isMatchingFileSize(final long fileSize) {
        return ((settings.getMaxSize() <= 0 || fileSize <= settings.getMaxSize())
                &&
                (settings.getMinSize() <= 0 || fileSize >= settings.getMinSize()))
    }

    boolean isMatchingLastMod(final Instant lastMod) {
        return ((settings.getMaxLastMod() == null
                || lastMod <= settings.getMaxLastMod().toInstant(ZoneOffset.UTC)
                &&
                (settings.getMinLastMod() == null
                        || lastMod >= settings.getMinLastMod().toInstant(ZoneOffset.UTC))))
    }

    boolean isMatchingFileResult(final FileResult fr) {
        return hasMatchingExtension(fr)
                && isMatchingFileName(fr.path.fileName.toString())
                && isMatchingFileType(fr.fileType)
                && isMatchingFileSize(fr.fileSize)
                && isMatchingLastMod(fr.lastMod == null ? null : fr.lastMod.toInstant())
    }

    boolean isMatchingArchiveFile(final Path path) {
        def fileName = path.fileName.toString()
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
        return (settings.inArchiveFilePatterns.empty
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

        def fileResult = new FileResult(path, fileTypes.getFileType(path), fileSize, lastMod)
        if (fileResult.fileType == FileType.ARCHIVE) {
            if ((settings.includeArchives || settings.archivesOnly) && isMatchingArchiveFile(path)) {
                return Optional.of(fileResult)
            }
            return Optional.empty()
        }
        if (!settings.archivesOnly && isMatchingFileResult(fileResult)) {
            return Optional.of(fileResult)
        }
        return Optional.empty()
    }

    final void sortFileResults(List<FileResult> fileResults) {
        if (settings.sortBy == SortBy.FILENAME) {
            fileResults.sort((fr1, fr2) -> fr1.compareByName(fr2, settings.sortCaseInsensitive))
        } else if (settings.sortBy == SortBy.FILESIZE) {
            fileResults.sort((fr1, fr2) -> fr1.compareBySize(fr2, settings.sortCaseInsensitive))
        } else if (settings.sortBy == SortBy.FILETYPE) {
            fileResults.sort((fr1, fr2) -> fr1.compareByType(fr2, settings.sortCaseInsensitive))
        } else if (settings.sortBy == SortBy.LASTMOD) {
            fileResults.sort((fr1, fr2) -> fr1.compareByLastMod(fr2, settings.sortCaseInsensitive))
        } else {
            fileResults.sort((fr1, fr2) -> fr1.compareByPath(fr2, settings.sortCaseInsensitive))
        }
        if (settings.sortDescending) {
            Collections.reverse(fileResults)
        }
    }

    private List<FileResult> recFindPath(final Path filePath, int minDepth, int maxDepth, int currentDepth) {
        def pathResults = new ArrayList<FileResult>()
        if (maxDepth > -1 && currentDepth > maxDepth) {
            return pathResults
        }
        def pathDirs = new ArrayList<Path>();
        try (DirectoryStream<Path> pathContents = Files.newDirectoryStream(filePath)) {
            for (Path path : pathContents) {
                if (Files.isDirectory(path) && isMatchingDir(path)) {
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
        return pathResults
    }

    private List<FileResult> findPath(final Path filePath) {
        if (Files.isDirectory(filePath)) {
            if (settings.getRecursive()) {
                return recFindPath(filePath, settings.getMinDepth(), settings.getMaxDepth(), 1)
            }
            return recFindPath(filePath, settings.getMinDepth(), 1, 1)
        } else if (Files.isRegularFile(filePath)) {
            Optional<FileResult> optFileResult = filterToFileResult(filePath);
            if (optFileResult.isPresent()) {
                return List.of(optFileResult.get())
            }
        }
        return new ArrayList<>();
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
            if (Files.isDirectory(path)) {
                // if maxDepth is zero, we can skip since a directory cannot be a result
                if (settings.maxDepth != 0) {
                    if (isMatchingDir(path)) {
                        // TODO: the findPath call is returning results, but for some reason the future result is null
                        fileResults.addAll(findPath(path))
//                        futures.add(executorService.submit{findPath(path)} as Future<List<FileResult>>)
                    } else {
                        throw new FindException('Startpath does not match find settings')
                    }
                }
            } else if (Files.isRegularFile(path)) {
                // if minDepth > zero, we can skip since the file is at depth zero
                if (settings.minDepth <= 0) {
                    if (isMatchingDir(path.getParent())) {
                        // TODO: the findPath call is returning results, but for some reason the future result is null
                        fileResults.addAll(findPath(path))
//                        futures.add(executorService.submit{findPath(path)} as Future<List<FileResult>>)
                    } else {
                        throw new FindException('Startpath does not match find settings')
                    }
                }
            } else {
                throw new FindException('Startpath is not a findable file type')
            }
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
        return fileResults
    }
}
