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
        FileResult fr = new FileResult(path, fileTypes.getFileType(path));
        return isMatchingFileResult(fr);
    }

    boolean isMatchingFileResult(final FileResult fr) {
        String fileName = fr.getPath().getFileName().toString();
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
            BasicFileAttributes stat = fr.getStat();
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
        for (String p : settings.getPaths()) {
            Path path = Paths.get(p);
            if (Files.isDirectory(path)) {
                if (isMatchingDir(path)) {
                    fileResults.addAll(findPath(path));
                } else {
                    throw new FindException("Startpath does not match find settings");
                }
            } else if (Files.isRegularFile(path)) {
                Optional<FileResult> optFileResult = filterToFileResult(path);
                if (optFileResult.isPresent()) {
                    fileResults.add(optFileResult.get());
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
