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
import java.util.*;
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
        Set<String> paths = settings.getPaths();
        if (paths.isEmpty()) {
            throw new FindException("Startpath not defined");
        }
        for (String p : paths) {
            Path path = Paths.get(p);
            if (!Files.exists(path)) {
                throw new FindException("Startpath not found");
            }
            if (!Files.isReadable(path)) {
                throw new FindException("Startpath not readable");
            }
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
                if (Files.isHidden(path)) {
                    return false;
                }
            } catch (IOException e) {
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
        FileResult sf = new FileResult(path, fileTypes.getFileType(path));
        return isMatchingFile(sf);
    }

    boolean isMatchingFile(final FileResult fr) {
        String fileName = fr.getPath().getFileName().toString();
        if (!settings.getInExtensions().isEmpty() || !settings.getOutExtensions().isEmpty()) {
            String ext = FileUtil.getExtension(fileName);
            if ((!settings.getInExtensions().isEmpty() && !settings.getInExtensions().contains(ext))
                    ||
                    (!settings.getOutExtensions().isEmpty() && settings.getOutExtensions().contains(ext))) {
                return false;
            }
        }
        return (settings.getInFilePatterns().isEmpty()
                ||
                matchesAnyPattern(fileName, settings.getInFilePatterns()))
               &&
               (settings.getOutFilePatterns().isEmpty()
                ||
                !matchesAnyPattern(fileName, settings.getOutFilePatterns()))
               &&
               (settings.getInFileTypes().isEmpty()
                ||
                settings.getInFileTypes().contains(fr.getFileType()))
               &&
               (settings.getOutFileTypes().isEmpty()
                ||
                !settings.getOutFileTypes().contains(fr.getFileType()));
    }

    boolean isMatchingArchiveFile(final Path path) {
        String fileName = path.getFileName().toString();
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

    FileResult filterToFileResult(final Path path) {
        if (settings.getExcludeHidden()) {
            try {
                if (Files.isHidden(path)) {
                    return null;
                }
            } catch (IOException e) {
                Logger.logError(e.getMessage());
                return null;
            }
        }

        FileResult fileResult = new FileResult(path, fileTypes.getFileType(path));
        if (fileResult.getFileType() == FileType.ARCHIVE) {
            if ((settings.getIncludeArchives() || settings.getArchivesOnly()) && isMatchingArchiveFile(path)) {
                return fileResult;
            }
            return null;
        }
        if (!settings.getArchivesOnly() && isMatchingFile(fileResult)) {
            return fileResult;
        }
        return null;
    }

    public final void sortFileResults(List<FileResult> fileResults) {
        if (settings.getSortBy().equals(SortBy.FILENAME)) {
            fileResults.sort(FileResult::compareByName);
        } else if (settings.getSortBy().equals(SortBy.FILETYPE)) {
            fileResults.sort(FileResult::compareByType);
        } else {
            fileResults.sort(FileResult::compareByPath);
        }
        if (settings.getSortDescending()) {
            Collections.reverse(fileResults);
        }
    }

    public final List<FileResult> find() throws FindException {
        List<FileResult> fileResults = new ArrayList<>();
        for (String p : settings.getPaths()) {
            Path path = Paths.get(p);
            if (Files.isDirectory(path)) {
                if (isMatchingDir(path)) {
                    fileResults.addAll(findPath(path));
                } else {
                    throw new FindException("Startpath does not match find settings");
                }
            } else if (Files.isRegularFile(path)) {
                FileResult fileResult = filterToFileResult(path);
                if (fileResult != null) {
                    fileResults.add(fileResult);
                } else {
                    throw new FindException("Startpath does not match find settings");
                }
            } else {
                throw new FindException("Startpath is not a findable file type");
            }
        }
        sortFileResults(fileResults);
        return fileResults;
    }

    private static class FindFileResultsVisitor extends SimpleFileVisitor<Path> {
        Function<Path, Boolean> filterDir;
        Function<Path, FileResult> filterToFileResult;
        List<FileResult> fileResults;

        FindFileResultsVisitor(final Function<Path, Boolean> filterDir,
                               final Function<Path, FileResult> filterToFileResult) {
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
                FileResult fr = filterToFileResult.apply(path);
                if (fr != null) {
                    fileResults.add(fr);
                }
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

    private List<FileResult> findPath(final Path filePath) {
        FindFileResultsVisitor findFileResultsVisitor = new FindFileResultsVisitor(this::isMatchingDir,
                this::filterToFileResult);

        // walk file tree to find files
        try {
            Files.walkFileTree(filePath, findFileResultsVisitor);
        } catch (IOException e) {
            e.printStackTrace();
        }

        return findFileResultsVisitor.fileResults;
    }
}
