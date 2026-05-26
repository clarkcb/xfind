/*******************************************************************************
Finder

Class to find matching files

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.*;
import java.util.concurrent.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static javafind.FindError.*;
import static javafind.Logger.log;

public class Finder {

    final private FindSettings settings;
    final private FileTypes fileTypes;

    public Finder(final FindSettings settings) {
        this.settings = settings;
        this.fileTypes = new FileTypes();
    }

    public final void validateSettings() throws FindException {
        var paths = settings.getPaths();
        if (null == paths || paths.isEmpty()) {
            throw new FindException(STARTPATH_NOT_DEFINED.getMessage());
        }
        for (var path : paths) {
            if (path == null || path.toString().isEmpty()) {
                throw new FindException(STARTPATH_NOT_DEFINED.getMessage());
            }
            if (!Files.exists(path)) {
                path = FileUtil.expandPath(path);
                if (!Files.exists(path)) {
                    throw new FindException(STARTPATH_NOT_FOUND.getMessage());
                }
            }
            if (!Files.isReadable(path)) {
                throw new FindException(STARTPATH_NOT_READABLE.getMessage());
            }
            if (Files.isSymbolicLink(path)) {
                if (!settings.getFollowSymlinks()) {
                    throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.getMessage());
                }
            } else if (Files.isDirectory(path)) {
                if (!isTraversableDirPath(path)) {
                    throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.getMessage());
                }
            } else if (Files.isRegularFile(path)) {
                if (filterToFileResult(path).isEmpty()) {
                    throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.getMessage());
                }
            } else {
                // TODO: start path is unknown/invalid type
                throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.getMessage());
            }
        }
        if (settings.getMaxDepth() > -1 && settings.getMaxDepth() < settings.getMinDepth()) {
            throw new FindException(INVALID_RANGE_FOR_MINDEPTH_AND_MAXDEPTH.getMessage());
        }
        if (settings.getMaxLastMod().isPresent() && settings.getMinLastMod().isPresent()
                && settings.getMaxLastMod().get().toInstant(ZoneOffset.UTC)
                .compareTo(settings.getMinLastMod().get().toInstant(ZoneOffset.UTC)) < 0) {
            throw new FindException(INVALID_RANGE_FOR_MINLASTMOD_AND_MAXLASTMOD.getMessage());
        }
        if (settings.getMaxSize() > 0 && settings.getMaxSize() < settings.getMinSize()) {
            throw new FindException(INVALID_RANGE_FOR_MINSIZE_AND_MAXSIZE.getMessage());
        }
    }

    private static boolean matchesAnyPattern(final String s, final Set<Pattern> patternSet) {
        return null != s && patternSet.stream().anyMatch(p -> p.matcher(s).find());
    }

    private boolean anyMatchesAnyPattern(final List<String> sList, final Set<Pattern> patternSet) {
        return sList.stream().anyMatch(s -> matchesAnyPattern(s, patternSet));
    }

    private boolean emptyOrMatchesAnyPattern(final String s, final Set<Pattern> patternSet) {
        return patternSet.isEmpty() || matchesAnyPattern(s, patternSet);
    }

    private boolean emptyOrNotMatchesAnyPattern(final String s, final Set<Pattern> patternSet) {
        return patternSet.isEmpty() || !matchesAnyPattern(s, patternSet);
    }

    private boolean emptyOrAnyMatchesAnyPattern(final List<String> sList, final Set<Pattern> patternSet) {
        return patternSet.isEmpty() || anyMatchesAnyPattern(sList, patternSet);
    }

    private boolean emptyOrNotAnyMatchesAnyPattern(final List<String> sList, final Set<Pattern> patternSet) {
        return patternSet.isEmpty() || !anyMatchesAnyPattern(sList, patternSet);
    }

    private boolean emptyOrMatchesAnyString(final String s, final Set<String> stringSet) {
        return stringSet.isEmpty() || stringSet.contains(s);
    }

    private boolean emptyOrNotMatchesAnyString(final String s, final Set<String> stringSet) {
        return stringSet.isEmpty() || !stringSet.contains(s);
    }

    private boolean emptyOrMatchesAnyFileType(final FileType fileType, final Set<FileType> fileTypeSet) {
        return fileTypeSet.isEmpty() || fileTypeSet.contains(fileType);
    }

    private boolean emptyOrNotMatchesAnyFileType(final FileType fileType, final Set<FileType> fileTypeSet) {
        return fileTypeSet.isEmpty() || !fileTypeSet.contains(fileType);
    }

    boolean isMatchingPathBySymlink(final Path path) {
        return settings.getFollowSymlinks() || !Files.isSymbolicLink(path);
    }

    boolean isMatchingDirPathByHidden(final Path dirPath) {
        return settings.getIncludeHidden() || !FileUtil.isHiddenPath(dirPath);
    }

    boolean isMatchingDirPathByInPatterns(final Path dirPath) {
        return emptyOrAnyMatchesAnyPattern(FileUtil.splitPath(dirPath), settings.getInDirPatterns());
    }

    boolean isMatchingDirPathByOutPatterns(final Path dirPath) {
        return emptyOrNotAnyMatchesAnyPattern(FileUtil.splitPath(dirPath), settings.getOutDirPatterns());
    }

    boolean isTraversableDirPath(final Path dirPath) {
        return isMatchingPathBySymlink(dirPath)
                && isMatchingDirPathByHidden(dirPath)
                && isMatchingDirPathByOutPatterns(dirPath);
    }

    boolean isMatchingDirPath(final Path dirPath) {
        return isMatchingPathBySymlink(dirPath)
                && isMatchingDirPathByHidden(dirPath)
                && isMatchingDirPathByInPatterns(dirPath)
                && isMatchingDirPathByOutPatterns(dirPath);
    }

    boolean isNullOrMatchingDirPath(final Path dirPath) {
        // null or empty dirPath is a match
        if (null == dirPath || dirPath.toString().isEmpty()) {
            return true;
        }
        return isMatchingDirPath(dirPath);
    }

    boolean isMatchingFileNameByHidden(final String fileName) {
        return settings.getIncludeHidden() || !FileUtil.isHiddenName(fileName);
    }

    boolean isMatchingArchiveExtension(final String ext) {
        return emptyOrMatchesAnyString(ext, settings.getInArchiveExtensions())
                && emptyOrNotMatchesAnyString(ext, settings.getOutArchiveExtensions());
    }

    boolean isMatchingArchiveExtensionForFilePath(final Path filePath) {
        if (!settings.getInArchiveExtensions().isEmpty() || !settings.getOutArchiveExtensions().isEmpty()) {
            return isMatchingArchiveExtension(FileUtil.getExtension(filePath.getFileName().toString()));
        }
        return true;
    }

    boolean isMatchingArchiveFileName(final String fileName) {
        return emptyOrMatchesAnyPattern(fileName, settings.getInArchiveFilePatterns())
                && emptyOrNotMatchesAnyPattern(fileName, settings.getOutArchiveFilePatterns());
    }

    boolean isMatchingArchiveFileNameForFilePath(final Path filePath) {
        if (!settings.getInArchiveFilePatterns().isEmpty() || !settings.getOutArchiveFilePatterns().isEmpty()) {
            var fileName = filePath.getFileName().toString();
            return isMatchingArchiveFileName(fileName);
        }
        return true;
    }

    boolean isMatchingArchiveFilePath(final Path filePath) {
        return isMatchingArchiveExtensionForFilePath(filePath)
                && isMatchingArchiveFileNameForFilePath(filePath);
    }

    // TODO: add tests
    boolean isMatchingArchiveFileResult(final FileResult fr) {
        return isMatchingArchiveFilePath(fr.path());
    }

    boolean isMatchingExtension(final String ext) {
        return emptyOrMatchesAnyString(ext, settings.getInExtensions())
                && emptyOrNotMatchesAnyString(ext, settings.getOutExtensions());
    }

    boolean isMatchingExtensionForFilePath(final Path filePath) {
        if (!settings.getInExtensions().isEmpty() || !settings.getOutExtensions().isEmpty()) {
            return isMatchingExtension(FileUtil.getExtension(filePath));
        }
        return true;
    }

    boolean isMatchingFileName(final String fileName) {
        return emptyOrMatchesAnyPattern(fileName, settings.getInFilePatterns())
                && emptyOrNotMatchesAnyPattern(fileName, settings.getOutFilePatterns());
    }

    boolean isMatchingFileNameForFilePath(final Path filePath) {
        if (!settings.getInFilePatterns().isEmpty() || !settings.getOutFilePatterns().isEmpty()) {
            return isMatchingFileName(filePath.getFileName().toString());
        }
        return true;
    }

    boolean isMatchingFilePath(final Path filePath) {
        return isNullOrMatchingDirPath(filePath.getParent())
                && isMatchingFileNameByHidden(filePath.getFileName().toString())
                && isMatchingExtensionForFilePath(filePath)
                && isMatchingFileNameForFilePath(filePath);
    }

    boolean isMatchingFileType(final FileType fileType) {
        return emptyOrMatchesAnyFileType(fileType, settings.getInFileTypes())
                && emptyOrNotMatchesAnyFileType(fileType, settings.getOutFileTypes());
    }

    boolean isMatchingFileSize(final long fileSize) {
        return ((settings.getMaxSize() <= 0 || fileSize <= settings.getMaxSize())
                && (settings.getMinSize() <= 0 || fileSize >= settings.getMinSize()));
    }

    boolean isMatchingLastMod(final Instant lastMod) {
        if (settings.getMaxLastMod().isEmpty() && settings.getMinLastMod().isEmpty()) {
            return true;
        }
        if (lastMod == null) {
            return false;
        }
        if (settings.getMaxLastMod().isPresent()
                && lastMod.compareTo(settings.getMaxLastMod().get().toInstant(ZoneOffset.UTC)) > 0) {
            return false;
        }
        return settings.getMinLastMod().isEmpty()
                || lastMod.compareTo(settings.getMinLastMod().get().toInstant(ZoneOffset.UTC)) >= 0;
    }

    boolean isMatchingLastMod(final FileTime lastMod) {
        var lastModInstant = lastMod == null ? null : lastMod.toInstant();
        return isMatchingLastMod(lastModInstant);
    }

    boolean isMatchingFileResult(final FileResult fr) {
        return isMatchingFilePath(fr.path())
                && isMatchingFileType(fr.fileType())
                && isMatchingFileSize(fr.fileSize())
                && isMatchingLastMod(fr.lastMod());
    }

    Optional<FileResult> filterArchiveFilePathToFileResult(final Path filePath, final FileType fileType) {
        if (!settings.getIncludeArchives() && !settings.getArchivesOnly()) {
            return Optional.empty();
        }
        if (!isMatchingArchiveFilePath(filePath)) {
            return Optional.empty();
        }
        return Optional.of(new FileResult(filePath, fileType, 0L, null));
    }

    Optional<FileResult> filterRegularFilePathToFileResult(final Path filePath, final FileType fileType) {
        if (settings.getArchivesOnly()) {
            return Optional.empty();
        }

        if (!isMatchingFilePath(filePath) || !isMatchingFileType(fileType)) {
            return Optional.empty();
        }

        long fileSize = 0L;
        FileTime lastMod = null;
        if (settings.needLastMod() || settings.needSize()) {
            try {
                BasicFileAttributes stat = Files.readAttributes(filePath, BasicFileAttributes.class);
                if (settings.needSize()) {
                    fileSize = stat.size();
                }
                if (settings.needLastMod()) {
                    lastMod = stat.lastModifiedTime();
                }
            } catch (IOException e) {
                Logger.logError(e.getMessage());
                return Optional.empty();
            }
        }

        if (!isMatchingFileSize(fileSize) || !isMatchingLastMod(lastMod)) {
            return Optional.empty();
        }

        return Optional.of(new FileResult(filePath, fileType, fileSize, lastMod));
    }

    Optional<FileResult> filterToFileResult(final Path filePath) {
        if (!isNullOrMatchingDirPath(filePath.getParent())
                || !isMatchingFileNameByHidden(filePath.getFileName().toString())) {
            return Optional.empty();
        }

        FileType fileType = fileTypes.getFileType(filePath);
        if (fileType == FileType.ARCHIVE) {
            return filterArchiveFilePathToFileResult(filePath, fileType);
        }

        return filterRegularFilePathToFileResult(filePath, fileType);
    }

    private List<FileResult> recFindPath(final Path path, int minDepth, int maxDepth, int currentDepth) {
        var pathResults = new ArrayList<FileResult>();
        var recurse = true;
        if (currentDepth == maxDepth) {
            recurse = false;
        } else if (maxDepth > -1 && currentDepth > maxDepth) {
            return pathResults;
        }
        List<Path> pathDirs = new ArrayList<>();
        try (DirectoryStream<Path> subPathStream = Files.newDirectoryStream(path)) {
            for (var subPath : subPathStream) {
                if (!isMatchingPathBySymlink(subPath)) {
                    continue;
                }
                if (Files.isDirectory(subPath) && recurse && isTraversableDirPath(subPath)) {
                    pathDirs.add(subPath);
                } else if (Files.isRegularFile(subPath) && (minDepth < 0 || currentDepth >= minDepth)) {
                    filterToFileResult(subPath).ifPresent(pathResults::add);
                }
            }
        } catch (IOException e) {
            Logger.logError(e.getMessage());
        }

        for (var pathDir : pathDirs) {
            pathResults.addAll(recFindPath(pathDir, minDepth, maxDepth, currentDepth + 1));
        }

        return pathResults;
    }

    private List<FileResult> findPath(Path path) throws FindException {
        if (!Files.exists(path)) {
            path = FileUtil.expandPath(path);
            if (!Files.exists(path)) {
                throw new FindException(STARTPATH_NOT_FOUND.getMessage());
            }
        }
        if (Files.isSymbolicLink(path) && !settings.getFollowSymlinks()) {
            throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.getMessage());
        }
        if (Files.isDirectory(path)) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (settings.getMaxDepth() == 0) {
                return Collections.emptyList();
            }
            if (isTraversableDirPath(path)) {
                int maxDepth = settings.getMaxDepth();
                if (!settings.getRecursive()) {
                    maxDepth = 1;
                }
                return recFindPath(path, settings.getMinDepth(), maxDepth, 1);
            } else {
                throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.getMessage());
            }
        } else if (Files.isRegularFile(path)) {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (settings.getMinDepth() > 0) {
                return Collections.emptyList();
            }
            var optFileResult = filterToFileResult(path);
            if (optFileResult.isPresent()) {
                return List.of(optFileResult.get());
            } else {
                throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.getMessage());
            }
        } else {
            throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.getMessage());
        }
    }

    private List<FileResult> findAsync() throws FindException {
        var futures = settings.getPaths().stream()
                .map(path -> CompletableFuture.supplyAsync(() -> {
                    try {
                        return findPath(path);
                    } catch (FindException e) {
                        throw new CompletionException(e);
                    }
                }))
                .toList();

        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();

        try {
            return futures.stream()
                    .map(CompletableFuture::join)
                    .flatMap(List::stream)
                    .toList();
        } catch (CompletionException e) {
            throw new FindException(e.getCause().getMessage());
        }
    }

    public final List<FileResult> find() throws FindException {
        List<FileResult> fileResults;
        // Don't bother with async unless we have more than one path
        if (settings.getPaths().size() == 1) {
            fileResults = findPath(settings.getPaths().iterator().next());
        } else {
            fileResults = findAsync();
        }
        if (fileResults.size() > 1) {
            var fileResultSorter = new FileResultSorter(settings);
            fileResultSorter.sort(new ArrayList<>(fileResults));
        }
        return fileResults;
    }

    public static List<Path> getMatchingDirs(final List<FileResult> results) {
        if (null == results || results.isEmpty()) {
            return Collections.emptyList();
        }
        return results.stream()
                .map(fr -> fr.path().getParent()).distinct()
                .sorted().collect(Collectors.toList());
    }

    public static void printMatchingDirs(final List<FileResult> results, final FileResultFormatter formatter) {
        var dirs = getMatchingDirs(results);
        if (!dirs.isEmpty()) {
            log(String.format("\nMatching directories (%d):", dirs.size()));
            for (var d : dirs) {
                log(formatter.formatDirPath(d));
            }
        } else {
            log("\nMatching directories: 0");
        }
    }

    public static void printMatchingFiles(final List<FileResult> results, final FileResultFormatter formatter) {
        if (null != results && !results.isEmpty()) {
            log(String.format("\nMatching files (%d):", results.size()));
            for (var f : results) {
                log(formatter.formatFileResult(f));
            }
        } else {
            log("\nMatching files: 0");
        }
    }
}
