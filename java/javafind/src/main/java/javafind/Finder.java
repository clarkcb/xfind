/*******************************************************************************
Finder

Class to find matching files

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.function.Function;
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

        FileResult fileResult = new FileResult(path, fileTypes.getFileType(path), fileSize, lastMod);
        if (fileResult.getFileType() == FileType.ARCHIVE) {
            if ((settings.getIncludeArchives() || settings.getArchivesOnly()) && isMatchingArchiveFile(path)) {
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

    public final List<FileResult> find() throws FindException {
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
            if (Files.isDirectory(path)) {
                // if maxDepth is zero, we can skip since a directory cannot be a result
                if (settings.getMaxDepth() != 0) {
                    if (isMatchingDir(path)) {
                        futures.add(executorService.submit(() -> findPath(path)));
                    } else {
                        throw new FindException("Startpath does not match find settings");
                    }
                }
            } else if (Files.isRegularFile(path)) {
                // if minDepth > zero, we can skip since the file is at depth zero
                if (settings.getMinDepth() <= 0) {
                    Optional<FileResult> optFileResult = filterToFileResult(path);
                    if (optFileResult.isPresent()) {
                        fileResults.add(optFileResult.get());
                    } else {
                        throw new FindException("Startpath does not match find settings");
                    }
                }
            } else {
                throw new FindException("Startpath is not a findable file type");
            }
        }

        for (Future<List<FileResult>> future : futures) {
            try {
                fileResults.addAll(future.get());
            } catch (InterruptedException | ExecutionException e) {
                throw new RuntimeException(e);
            }
        }

        executorService.shutdown();

        sortFileResults(fileResults);
        return fileResults;
    }

    private static class FindFileResultsVisitor extends SimpleFileVisitor<Path> {
        Function<Path, Boolean> filterDir;
        Function<Path, Optional<FileResult>> filterToFileResult;
        List<FileResult> fileResults;

        FindFileResultsVisitor(final Function<Path, Boolean> filterDir,
                               final Function<Path, Optional<FileResult>> filterToFileResult) {
            super();
            this.filterDir = filterDir;
            this.filterToFileResult = filterToFileResult;
            fileResults = new ArrayList<>();
        }

        @Override
        public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
            Objects.requireNonNull(dir);
            Objects.requireNonNull(attrs);
            if (filterDir.apply(dir)) {
                return FileVisitResult.CONTINUE;
            }
            return FileVisitResult.SKIP_SUBTREE;
        }

        @Override
        public FileVisitResult visitFile(Path path, BasicFileAttributes attrs) {
            Objects.requireNonNull(path);
            Objects.requireNonNull(attrs);
            if (attrs.isRegularFile()) {
                Optional<FileResult> optFileResult = filterToFileResult.apply(path);
                optFileResult.ifPresent(fileResult -> fileResults.add(fileResult));
            }
            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult visitFileFailed(Path path, IOException exc) {
            System.err.println(exc.getMessage());
            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
            if (exc != null)
                throw exc;
            return FileVisitResult.CONTINUE;
        }
    }

    private boolean filterDir(final Path dirPath, long startPathSepCount) {
        if (dirPath == null) return true;
        var pathSepCount = FileUtil.getSepCount(dirPath);
        var depth = (int)(pathSepCount - startPathSepCount);
        return (settings.getMaxDepth() < 1 || depth <= settings.getMaxDepth())
                && isMatchingDir(dirPath);
    }

    private Optional<FileResult> filterFile(final Path filePath, long startPathSepCount) {
        var pathSepCount = FileUtil.getSepCount(filePath);
        var depth = (int)(pathSepCount - startPathSepCount);
        if (depth < settings.getMinDepth() || (settings.getMaxDepth() > 0 && depth > settings.getMaxDepth())) {
            return Optional.empty();
        }
        return filterToFileResult(filePath);
    }

    private List<FileResult> findPath(final Path filePath) {
        var filePathSepCount = FileUtil.getSepCount(filePath);
        Function<Path, Boolean> filterDirFunc = (dirPath) -> filterDir(dirPath, filePathSepCount);
        Function<Path, Optional<FileResult>> filterFileFunc = (path) -> filterFile(path, filePathSepCount);
        var findFileResultsVisitor = new FindFileResultsVisitor(filterDirFunc, filterFileFunc);

        // walk file tree to find files
        try {
            Files.walkFileTree(filePath, findFileResultsVisitor);
        } catch (IOException e) {
            e.printStackTrace();
        }

        return findFileResultsVisitor.fileResults;
    }
}
