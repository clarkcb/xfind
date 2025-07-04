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

import static javafind.Logger.log;

public class Finder {

    public static final String STARTPATH_NOT_DEFINED = "Startpath not defined";
    public static final String STARTPATH_NOT_FOUND = "Startpath not found";
    public static final String STARTPATH_NOT_READABLE = "Startpath not readable";
    public static final String INVALID_RANGE_FOR_MINDEPTH_AND_MAXDEPTH = "Invalid range for mindepth and maxdepth";
    public static final String INVALID_RANGE_FOR_MINLASTMOD_AND_MAXLASTMOD = "Invalid range for minlastmod and maxlastmod";
    public static final String INVALID_RANGE_FOR_MINSIZE_AND_MAXSIZE = "Invalid range for minsize and maxsize";
    public static final String STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS = "Startpath does not match find settings";

    final private FindSettings settings;
    final private FileTypes fileTypes;

    public Finder(final FindSettings settings) {
        this.settings = settings;
        this.fileTypes = new FileTypes();
    }

    public final void validateSettings() throws FindException {
        var paths = settings.getPaths();
        if (null == paths || paths.isEmpty() || paths.stream().anyMatch(p -> p == null || p.toString().isEmpty())) {
            throw new FindException(STARTPATH_NOT_DEFINED);
        }
        for (var path : paths) {
            if (!Files.exists(path)) {
                path = FileUtil.expandPath(path);
            }
            if (Files.exists(path)) {
                if (!Files.isReadable(path)) {
                    throw new FindException(STARTPATH_NOT_READABLE);
                }
            } else {
                throw new FindException(STARTPATH_NOT_FOUND);
            }
        }
        if (settings.getMaxDepth() > -1 && settings.getMaxDepth() < settings.getMinDepth()) {
            throw new FindException(INVALID_RANGE_FOR_MINDEPTH_AND_MAXDEPTH);
        }
        if (settings.getMaxLastMod().isPresent() && settings.getMinLastMod().isPresent()
                && settings.getMaxLastMod().get().toInstant(ZoneOffset.UTC)
                .compareTo(settings.getMinLastMod().get().toInstant(ZoneOffset.UTC)) < 0) {
            throw new FindException(INVALID_RANGE_FOR_MINLASTMOD_AND_MAXLASTMOD);
        }
        if (settings.getMaxSize() > 0 && settings.getMaxSize() < settings.getMinSize()) {
            throw new FindException(INVALID_RANGE_FOR_MINSIZE_AND_MAXSIZE);
        }
    }

    private boolean anyMatchesAnyPattern(final List<String> sList,
                                         final Set<Pattern> patternSet) {
        return sList.stream().anyMatch(s -> matchesAnyPattern(s, patternSet));
    }

    private static boolean matchesAnyPattern(final String s,
                                             final Set<Pattern> patternSet) {
        return null != s && patternSet.stream().anyMatch(p -> p.matcher(s).find());
    }

    boolean isMatchingDir(final Path path) {
        // null or empty path is a match
        if (null == path || path.toString().isEmpty()) {
            return true;
        }
        if (!settings.getIncludeHidden()) {
            try {
                // This erroneously returns true for . and ..
//                if (Files.isHidden(path)) {
//                    return false;
//                }
                if (FileUtil.isHidden(path)) {
                    return false;
                }
            } catch (Exception e) {
                Logger.logError(e.getMessage());
                return false;
            }
        }
        List<String> pathElems = FileUtil.splitPath(path);
        return (settings.getInDirPatterns().isEmpty()
                ||
                anyMatchesAnyPattern(pathElems, settings.getInDirPatterns()))
                &&
                (settings.getOutDirPatterns().isEmpty()
                 ||
                 !anyMatchesAnyPattern(pathElems, settings.getOutDirPatterns()));
    }

    boolean isMatchingExtension(final String ext) {
        return ((settings.getInExtensions().isEmpty()
                || settings.getInExtensions().contains(ext))
                &&
                (settings.getOutExtensions().isEmpty()
                        || !settings.getOutExtensions().contains(ext)));
    }

    boolean hasMatchingExtension(final FileResult fr) {
        if (!settings.getInExtensions().isEmpty() || !settings.getOutExtensions().isEmpty()) {
            var fileName = fr.path().getFileName().toString();
            var ext = FileUtil.getExtension(fileName);
            return isMatchingExtension(ext);
        }
        return true;
    }

    boolean isMatchingFileName(final String fileName) {
        return ((settings.getInFilePatterns().isEmpty()
                || matchesAnyPattern(fileName, settings.getInFilePatterns()))
                &&
                (settings.getOutFilePatterns().isEmpty()
                        || !matchesAnyPattern(fileName, settings.getOutFilePatterns())));
    }

    boolean isMatchingFileType(final FileType fileType) {
        return ((settings.getInFileTypes().isEmpty()
                || settings.getInFileTypes().contains(fileType))
                &&
                (settings.getOutFileTypes().isEmpty()
                        || !settings.getOutFileTypes().contains(fileType)));
    }

    boolean isMatchingFileSize(final long fileSize) {
        return ((settings.getMaxSize() <= 0 || fileSize <= settings.getMaxSize())
                &&
                (settings.getMinSize() <= 0 || fileSize >= settings.getMinSize()));
    }

    boolean isMatchingLastMod(final Instant lastMod) {
        return ((settings.getMaxLastMod().isEmpty()
                || lastMod.compareTo(settings.getMaxLastMod().get().toInstant(ZoneOffset.UTC)) <= 0
                &&
                (settings.getMinLastMod().isEmpty()
                        || lastMod.compareTo(settings.getMinLastMod().get().toInstant(ZoneOffset.UTC)) >= 0)));
    }

    boolean hasMatchingLastMod(final FileResult fr) {
        var lastMod = fr.lastMod() == null ? null : fr.lastMod().toInstant();
        return isMatchingLastMod(lastMod);
    }

    boolean isMatchingFileResult(final FileResult fr) {
        return hasMatchingExtension(fr)
                && isMatchingFileName(fr.path().getFileName().toString())
                && isMatchingFileType(fr.fileType())
                && isMatchingFileSize(fr.fileSize())
                && hasMatchingLastMod(fr);
    }

    boolean isMatchingArchiveFile(final Path path) {
        var fileName = path.getFileName().toString();
        if (!settings.getInArchiveExtensions().isEmpty() || !settings.getOutArchiveExtensions().isEmpty()) {
            var ext = FileUtil.getExtension(fileName);
            if ((!settings.getInArchiveExtensions().isEmpty() && !settings.getInArchiveExtensions().contains(ext))
                    ||
                    (!settings.getOutArchiveExtensions().isEmpty()
                            &&
                            settings.getOutArchiveExtensions().contains(ext))) {
                return false;
            }
        }
        return (settings.getInArchiveFilePatterns().isEmpty()
                ||
                matchesAnyPattern(fileName, settings.getInArchiveFilePatterns()))
               &&
               (settings.getOutArchiveFilePatterns().isEmpty()
                ||
                !matchesAnyPattern(fileName, settings.getOutArchiveFilePatterns()));
    }

    Optional<FileResult> filterToFileResult(final Path path) {
        Optional<FileResult> emptyFileResult = Optional.empty();
        if (!settings.getIncludeHidden()) {
            try {
                if (Files.isHidden(path)) {
                    return emptyFileResult;
                }
            } catch (IOException e) {
                Logger.logError(e.getMessage());
                return emptyFileResult;
            }
        }

        FileType fileType = fileTypes.getFileType(path);
        if (fileType == FileType.ARCHIVE && !settings.getIncludeArchives() && !settings.getArchivesOnly()) {
            return emptyFileResult;
        }

        long fileSize = 0L;
        FileTime lastMod = null;
        if (settings.needLastMod() || settings.needSize()) {
            try {
                BasicFileAttributes stat = Files.readAttributes(path, BasicFileAttributes.class);
                if (settings.needSize()) fileSize = stat.size();
                if (settings.needLastMod()) lastMod = stat.lastModifiedTime();
            } catch (IOException e) {
                Logger.logError(e.getMessage());
                return emptyFileResult;
            }
        }

        FileResult fileResult = new FileResult(path, fileType, fileSize, lastMod);
        if (fileResult.fileType() == FileType.ARCHIVE) {
            if (isMatchingArchiveFile(path)) {
                return Optional.of(fileResult);
            }
            return emptyFileResult;
        }
        if (!settings.getArchivesOnly() && isMatchingFileResult(fileResult)) {
            return Optional.of(fileResult);
        }
        return emptyFileResult;
    }

    private List<FileResult> recFindPath(final Path filePath, int minDepth, int maxDepth, int currentDepth) {
        var pathResults = new ArrayList<FileResult>();
        var recurse = true;
        if (currentDepth == maxDepth) {
            recurse = false;
        } else if (maxDepth > -1 && currentDepth > maxDepth) {
            return pathResults;
        }
        List<Path> pathDirs = new ArrayList<>();
        try (DirectoryStream<Path> pathContents = Files.newDirectoryStream(filePath)) {
            for (var path : pathContents) {
                if (Files.isSymbolicLink(path) && !settings.getFollowSymlinks()) {
                    continue;
                }
                if (Files.isDirectory(path) && recurse && isMatchingDir(path)) {
                    pathDirs.add(path);
                } else if (Files.isRegularFile(path) && (minDepth < 0 || currentDepth >= minDepth)) {
                    filterToFileResult(path).ifPresent(pathResults::add);
                }
            }
            for (var pathDir : pathDirs) {
                pathResults.addAll(recFindPath(pathDir, minDepth, maxDepth, currentDepth + 1));
            }
        } catch (IOException e) {
            Logger.logError(e.getMessage());
        }
        return pathResults;
    }

    private List<FileResult> findPath(Path filePath) throws FindException {
        if (!Files.exists(filePath)) {
            filePath = FileUtil.expandPath(filePath);
        }
        if (Files.isDirectory(filePath)) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (settings.getMaxDepth() == 0) {
                return Collections.emptyList();
            }
            if (isMatchingDir(filePath)) {
                int maxDepth = settings.getMaxDepth();
                if (!settings.getRecursive()) {
                    maxDepth = 1;
                }
                return recFindPath(filePath, settings.getMinDepth(), maxDepth, 1);
            } else {
                throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS);
            }
        } else {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (settings.getMinDepth() > 0) {
                return Collections.emptyList();
            }
            var optFileResult = filterToFileResult(filePath);
            if (optFileResult.isPresent()) {
                return List.of(optFileResult.get());
            } else {
                throw new FindException(STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS);
            }
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

        var allOfFuture = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));

        try {
            allOfFuture.join(); // Wait for all futures to complete
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
        var fileResultSorter = new FileResultSorter(settings);
        fileResultSorter.sort(fileResults);
        return fileResults;
    }

    public static List<Path> getMatchingDirs(final List<FileResult> results) {
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
        if (!results.isEmpty()) {
            log(String.format("\nMatching files (%d):", results.size()));
            for (var f : results) {
                log(formatter.formatFileResult(f));
            }
        } else {
            log("\nMatching files: 0");
        }
    }
}
