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
        if (null == paths || paths.isEmpty() || paths.stream().anyMatch(p -> p == null || p.isEmpty())) {
            throw new FindException("Startpath not defined");
        }
        for (var p : paths) {
            var path = Paths.get(p);
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
        if (settings.getExcludeHidden()) {
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

    // this is temporary to appease the tests
    boolean isMatchingFile(final Path path) {
        var fr = new FileResult(path, fileTypes.getFileType(path));
        return isMatchingFileResult(fr);
    }

    boolean isMatchingFileResult(final FileResult fr) {
        var fileName = fr.getPath().getFileName().toString();
        if (!settings.getInExtensions().isEmpty() || !settings.getOutExtensions().isEmpty()) {
            String ext = FileUtil.getExtension(fileName);
            if ((!settings.getInExtensions().isEmpty() && !settings.getInExtensions().contains(ext))
                    ||
                    (!settings.getOutExtensions().isEmpty() && settings.getOutExtensions().contains(ext))) {
                return false;
            }
        }
        if ((!settings.getInFilePatterns().isEmpty()
                && !matchesAnyPattern(fileName, settings.getInFilePatterns()))
                ||
                (!settings.getOutFilePatterns().isEmpty()
                        &&
                        matchesAnyPattern(fileName, settings.getOutFilePatterns()))) {
            return false;
        }
        if ((!settings.getInFileTypes().isEmpty()
                &&
                !settings.getInFileTypes().contains(fr.getFileType()))
                ||
                (!settings.getOutFileTypes().isEmpty()
                        &&
                        settings.getOutFileTypes().contains(fr.getFileType()))) {
            return false;
        }
        if (fr.getStat() != null) {
            var stat = fr.getStat();
            if ((settings.getMaxLastMod() != null
                    && stat.lastModifiedTime().toInstant().compareTo(settings.getMaxLastMod().toInstant(ZoneOffset.UTC)) > 0)
                    || (settings.getMinLastMod() != null
                    && stat.lastModifiedTime().toInstant().compareTo(settings.getMinLastMod().toInstant(ZoneOffset.UTC)) < 0)
                    || (settings.getMaxSize() > 0 && stat.size() > settings.getMaxSize())
                    || (settings.getMinSize() > 0 && stat.size() < settings.getMinSize())) {
                return false;
            }
        }
        return true;
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
        if (settings.getExcludeHidden()) {
            try {
                if (Files.isHidden(path)) {
                    return Optional.empty();
                }
            } catch (IOException e) {
                Logger.logError(e.getMessage());
                return Optional.empty();
            }
        }

        BasicFileAttributes stat = null;
        if (settings.needStat()) {
            try {
                stat = Files.readAttributes(path, BasicFileAttributes.class);
            } catch (IOException e) {
                Logger.logError(e.getMessage());
                return Optional.empty();
            }
        }

        FileResult fileResult = new FileResult(path, fileTypes.getFileType(path), stat);
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

    public final int compareFileResults(FileResult fr1, FileResult fr2) {
        if (settings.getSortBy().equals(SortBy.FILENAME)) {
            return fr1.compareByName(fr2, settings.getSortCaseInsensitive());
        } else if (settings.getSortBy().equals(SortBy.FILESIZE)) {
            return fr1.compareBySize(fr2, settings.getSortCaseInsensitive());
        } else if (settings.getSortBy().equals(SortBy.FILETYPE)) {
            return fr1.compareByType(fr2, settings.getSortCaseInsensitive());
        } else if (settings.getSortBy().equals(SortBy.LASTMOD)) {
            return fr1.compareByLastMod(fr2, settings.getSortCaseInsensitive());
        } else {
            return fr1.compareByPath(fr2, settings.getSortCaseInsensitive());
        }
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

        for (var p : settings.getPaths()) {
            var path = Paths.get(p);
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
