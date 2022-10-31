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
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
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
        List<String> pathElems = FileUtil.splitPath(path);
        if (settings.getExcludeHidden()
                && pathElems.stream().anyMatch(FileUtil::isHidden)) {
            return false;
        }
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

    boolean isMatchingFile(final FileResult sf) {
        String fileName = sf.getPath().getFileName().toString();
        String ext = FileUtil.getExtension(fileName);
        return (settings.getInExtensions().isEmpty()
                ||
                settings.getInExtensions().contains(ext))
               &&
               (settings.getOutExtensions().isEmpty()
                ||
                !settings.getOutExtensions().contains(ext))
               &&
               (settings.getInFilePatterns().isEmpty()
                ||
                matchesAnyPattern(fileName, settings.getInFilePatterns()))
               &&
               (settings.getOutFilePatterns().isEmpty()
                ||
                !matchesAnyPattern(fileName, settings.getOutFilePatterns()))
               &&
               (settings.getInFileTypes().isEmpty()
                ||
                settings.getInFileTypes().contains(sf.getFileType()))
               &&
               (settings.getOutFileTypes().isEmpty()
                ||
                !settings.getOutFileTypes().contains(sf.getFileType()));
    }

    boolean isMatchingArchiveFile(final Path path) {
        String ext = FileUtil.getExtension(path);
        return (settings.getInArchiveExtensions().isEmpty()
                ||
                settings.getInArchiveExtensions().contains(ext))
               &&
               (settings.getOutArchiveExtensions().isEmpty()
                ||
                !settings.getOutArchiveExtensions().contains(ext))
               &&
               (settings.getInArchiveFilePatterns().isEmpty()
                ||
                matchesAnyPattern(path.getFileName().toString(), settings.getInArchiveFilePatterns()))
               &&
               (settings.getOutArchiveFilePatterns().isEmpty()
                ||
                !matchesAnyPattern(path.getFileName().toString(), settings.getOutArchiveFilePatterns()));
    }

    boolean filterFile(final Path path) {
        if (FileUtil.isHidden(path) && settings.getExcludeHidden()) {
            return false;
        }
        if (fileTypes.getFileType(path) == FileType.ARCHIVE) {
            return settings.getIncludeArchives() && isMatchingArchiveFile(path);
        }
        return !settings.getArchivesOnly() && isMatchingFile(path);
    }

    FileResult filterToFileResult(final Path path) {
        if (FileUtil.isHidden(path) && settings.getExcludeHidden()) {
            return null;
        }
        FileResult fileResult = new FileResult(path, fileTypes.getFileType(path));
        if (fileResult.getFileType() == FileType.ARCHIVE) {
            if ((settings.getIncludeArchives() || settings.getArchivesOnly())
                && isMatchingArchiveFile(path)) {
                    return fileResult;
            }
            return null;
        }
        if (!settings.getArchivesOnly() && isMatchingFile(fileResult)) {
            return fileResult;
        }
        return null;
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
        return fileResults;
    }

    private static class FindFindFileVisitor extends SimpleFileVisitor<Path> {
        Function<Path, Boolean> filterDir;
        Function<Path, FileResult> filterToFindFile;
        List<FileResult> fileResults;

        FindFindFileVisitor(final Function<Path, Boolean> filterDir,
                            final Function<Path, FileResult> filterToFindFile) {
            super();
            this.filterDir = filterDir;
            this.filterToFindFile = filterToFindFile;
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
                FileResult fr = filterToFindFile.apply(path);
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
        FindFindFileVisitor findFindFileVisitor = new FindFindFileVisitor(this::isMatchingDir,
                this::filterToFileResult);

        // walk file tree to find files
        try {
            Files.walkFileTree(filePath, findFindFileVisitor);
        } catch (IOException e) {
            e.printStackTrace();
        }

        List<FileResult> fileResults = findFindFileVisitor.fileResults;
        return fileResults;
    }
}
