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
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.regex.Pattern;

public class Finder {

    final private FindSettings settings;
    final private FileTypes fileTypes;

    public Finder(final FindSettings settings) {
        this.settings = settings;
        this.fileTypes = new FileTypes();
    }

    final void validateSettings() throws FindException {
        var paths = settings.getPaths();
        if (null == paths || paths.isEmpty() || paths.stream().anyMatch(p -> p == null || p.toString().isEmpty())) {
            throw new FindException("Startpath not defined");
        }
        for (var path : paths) {
            if (!Files.exists(path)) {
                throw new FindException("Startpath not found");
            }
            if (!Files.isReadable(path)) {
                throw new FindException("Startpath not readable");
            }
        }
        if (settings.getMaxDepth() > -1 && settings.getMaxDepth() < settings.getMinDepth()) {
            throw new FindException("Invalid range for mindepth and maxdepth");
        }
        if (settings.getMaxLastMod() != null && settings.getMinLastMod() != null
                && settings.getMaxLastMod().toInstant(ZoneOffset.UTC).compareTo(settings.getMinLastMod().toInstant(ZoneOffset.UTC)) < 0) {
            throw new FindException("Invalid range for minlastmod and maxlastmod");
        }
        if (settings.getMaxSize() > 0 && settings.getMaxSize() < settings.getMinSize()) {
            throw new FindException("Invalid range for minsize and maxsize");
        }
    }

    private boolean anyMatchesAnyPattern(final List<String> sList,
                                         final Set<Pattern> patternSet) {
        return sList.stream().anyMatch(s -> matchesAnyPattern(s, patternSet));
    }

    private boolean matchesAnyPattern(final String s,
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
            var fileName = fr.getPath().getFileName().toString();
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
        return ((settings.getMaxLastMod() == null
                || lastMod.compareTo(settings.getMaxLastMod().toInstant(ZoneOffset.UTC)) <= 0
                &&
                (settings.getMinLastMod() == null
                        || lastMod.compareTo(settings.getMinLastMod().toInstant(ZoneOffset.UTC)) >= 0)));
    }

    boolean hasMatchingLastMod(final FileResult fr) {
        Instant lastMod = fr.getLastMod() == null ? null : fr.getLastMod().toInstant();
        return isMatchingLastMod(lastMod);
    }

    boolean isMatchingFileResult(final FileResult fr) {
        return hasMatchingExtension(fr)
                && isMatchingFileName(fr.getPath().getFileName().toString())
                && isMatchingFileType(fr.getFileType())
                && isMatchingFileSize(fr.getFileSize())
                && hasMatchingLastMod(fr);
    }

    boolean isMatchingArchiveFile(final Path path) {
        var fileName = path.getFileName().toString();
        if (!settings.getInArchiveExtensions().isEmpty() || !settings.getOutArchiveExtensions().isEmpty()) {
            String ext = FileUtil.getExtension(fileName);
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
        if (!settings.getIncludeHidden()) {
            try {
                if (Files.isHidden(path)) {
                    return Optional.empty();
                }
            } catch (IOException e) {
                Logger.logError(e.getMessage());
                return Optional.empty();
            }
        }

        FileType fileType = fileTypes.getFileType(path);
        if (fileType == FileType.ARCHIVE && !settings.getIncludeArchives() && !settings.getArchivesOnly()) {
            return Optional.empty();
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
                return Optional.empty();
            }
        }

        FileResult fileResult = new FileResult(path, fileType, fileSize, lastMod);
        if (fileResult.getFileType() == FileType.ARCHIVE) {
            if (isMatchingArchiveFile(path)) {
                return Optional.of(fileResult);
            }
            return Optional.empty();
        }
        if (!settings.getArchivesOnly() && isMatchingFileResult(fileResult)) {
            return Optional.of(fileResult);
        }
        return Optional.empty();
    }

    public final void sortFileResults(List<FileResult> fileResults) {
        if (settings.getSortBy().equals(SortBy.FILENAME)) {
            fileResults.sort((fr1, fr2) -> fr1.compareByName(fr2, settings.getSortCaseInsensitive()));
        } else if (settings.getSortBy().equals(SortBy.FILESIZE)) {
            fileResults.sort((fr1, fr2) -> fr1.compareBySize(fr2, settings.getSortCaseInsensitive()));
        } else if (settings.getSortBy().equals(SortBy.FILETYPE)) {
            fileResults.sort((fr1, fr2) -> fr1.compareByType(fr2, settings.getSortCaseInsensitive()));
        } else if (settings.getSortBy().equals(SortBy.LASTMOD)) {
            fileResults.sort((fr1, fr2) -> fr1.compareByLastMod(fr2, settings.getSortCaseInsensitive()));
        } else {
            fileResults.sort((fr1, fr2) -> fr1.compareByPath(fr2, settings.getSortCaseInsensitive()));
        }
        if (settings.getSortDescending()) {
            Collections.reverse(fileResults);
        }
    }

    private List<FileResult> recFindPath(final Path filePath, int minDepth, int maxDepth, int currentDepth) {
        List<FileResult> pathResults = new ArrayList<>();
        boolean recurse = true;
        if (currentDepth == maxDepth) {
            recurse = false;
        } else if (maxDepth > -1 && currentDepth > maxDepth) {
            return pathResults;
        }
        List<Path> pathDirs = new ArrayList<>();
        try (DirectoryStream<Path> pathContents = Files.newDirectoryStream(filePath)) {
            for (Path path : pathContents) {
                if (Files.isDirectory(path) && recurse && isMatchingDir(path)) {
                    pathDirs.add(path);
                } else if (Files.isRegularFile(path) && (minDepth < 0 || currentDepth >= minDepth)) {
                    Optional<FileResult> optFileResult = filterToFileResult(path);
                    optFileResult.ifPresent(pathResults::add);
                }
            }
            for (Path pathDir : pathDirs) {
                pathResults.addAll(recFindPath(pathDir, minDepth, maxDepth, currentDepth + 1));
            }
        } catch (IOException e) {
            Logger.logError(e.getMessage());
        }
        return pathResults;
    }

    private List<FileResult> findPath(final Path filePath) throws FindException {
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
                throw new FindException("Startpath does not match find settings");
            }
        } else if (Files.isRegularFile(filePath)) {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (settings.getMinDepth() > 0) {
                return Collections.emptyList();
            }
            Optional<FileResult> optFileResult = filterToFileResult(filePath);
            if (optFileResult.isPresent()) {
                return List.of(optFileResult.get());
            } else {
                throw new FindException("Startpath does not match find settings");
            }
        } else {
            throw new FindException("Startpath is not a findable file type");
        }
    }

    private List<FileResult> findAsync() throws FindException {
        List<FileResult> fileResults = new ArrayList<>();

        // get thread pool the size of the number of paths to find
        ExecutorService executorService;
        if (settings.getPaths().size() == 1) {
            executorService = Executors.newSingleThreadExecutor();
        } else {
            executorService = Executors.newFixedThreadPool(settings.getPaths().size());
        }
        List<Future<List<FileResult>>> futures = new ArrayList<>();

        for (var path : settings.getPaths()) {
            futures.add(executorService.submit(() -> findPath(path)));
        }

        for (Future<List<FileResult>> future : futures) {
            try {
                fileResults.addAll(future.get());
            } catch (InterruptedException | ExecutionException e) {
                throw new FindException(e.getCause().getMessage());
            }
        }

        executorService.shutdown();

        sortFileResults(fileResults);
        return fileResults;
    }

    public final List<FileResult> find() throws FindException {
        List<FileResult> fileResults = findAsync();
        sortFileResults(fileResults);
        return fileResults;
    }
}
