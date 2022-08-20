/*******************************************************************************
Finder

Class to find matching files

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
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
            File pFile = new File(p);
            if (!pFile.exists()) {
                throw new FindException("Startpath not found");
            }
            if (!pFile.canRead()) {
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

    boolean isMatchingDir(final File d) {
        List<String> pathElems = FileUtil.splitPath(d.toString());
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
    boolean isMatchingFile(final File f) {
        FileResult sf = new FileResult(f, fileTypes.getFileType(f));
        return isMatchingFile(sf);
    }

    boolean isMatchingFile(final FileResult sf) {
        String fileName = sf.getFile().getName();
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

    boolean isMatchingArchiveFile(final File f) {
        String ext = FileUtil.getExtension(f);
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
                matchesAnyPattern(f.getName(), settings.getInArchiveFilePatterns()))
               &&
               (settings.getOutArchiveFilePatterns().isEmpty()
                ||
                !matchesAnyPattern(f.getName(), settings.getOutArchiveFilePatterns()));
    }

    boolean filterFile(final File f) {
        if (FileUtil.isHidden(f) && settings.getExcludeHidden()) {
            return false;
        }
        if (fileTypes.getFileType(f) == FileType.ARCHIVE) {
            return settings.getIncludeArchives() && isMatchingArchiveFile(f);
        }
        return !settings.getArchivesOnly() && isMatchingFile(f);
    }

    FileResult filterToFileResult(final File f) {
        if (FileUtil.isHidden(f) && settings.getExcludeHidden()) {
            return null;
        }
        FileResult fileResult = new FileResult(f, fileTypes.getFileType(f));
        if (fileTypes.getFileType(f) == FileType.ARCHIVE) {
            if ((settings.getIncludeArchives() || settings.getArchivesOnly())
                && isMatchingArchiveFile(f)) {
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
            File pFile = new File(p);
            if (pFile.isDirectory()) {
                if (isMatchingDir(pFile)) {
                    fileResults.addAll(findPath(pFile));
                } else {
                    throw new FindException("Startpath does not match find settings");
                }
            } else if (pFile.isFile()) {
                FileResult fileResult = filterToFileResult(pFile);
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
        Function<File, Boolean> filterDir;
        Function<File, FileResult> filterToFindFile;
        List<FileResult> fileResults;

        FindFindFileVisitor(final Function<File, Boolean> filterDir,
                            final Function<File, FileResult> filterToFindFile) {
            super();
            this.filterDir = filterDir;
            this.filterToFindFile = filterToFindFile;
            fileResults = new ArrayList<>();
        }

        @Override
        public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
            Objects.requireNonNull(dir);
            Objects.requireNonNull(attrs);
            if (filterDir.apply(dir.toFile())) {
                return FileVisitResult.CONTINUE;
            }
            return FileVisitResult.SKIP_SUBTREE;
        }

        @Override
        public FileVisitResult visitFile(Path path, BasicFileAttributes attrs) {
            Objects.requireNonNull(path);
            Objects.requireNonNull(attrs);
            if (attrs.isRegularFile()) {
                File f = path.toFile();
                FileResult ff = filterToFindFile.apply(f);
                if (ff != null) {
                    fileResults.add(ff);
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

    private List<FileResult> findPath(final File filePath) {
        Path startPath = filePath.toPath();
        FindFindFileVisitor findFindFileVisitor = new FindFindFileVisitor(this::isMatchingDir,
                this::filterToFileResult);

        // walk file tree to find files
        try {
            Files.walkFileTree(startPath, findFindFileVisitor);
        } catch (IOException e) {
            e.printStackTrace();
        }

        List<FileResult> fileResults = findFindFileVisitor.fileResults;
        return fileResults;
    }
}
