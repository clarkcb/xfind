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

    boolean isFindDir(final File d) {
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
    boolean isFindFile(final File f) {
        FindFile sf = new FindFile(f.getParent(), f.getName(), fileTypes.getFileType(f));
        return isFindFile(sf);
    }

    boolean isFindFile(final FindFile sf) {
        String ext = FileUtil.getExtension(sf.getFileName());
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
                matchesAnyPattern(sf.getFileName(), settings.getInFilePatterns()))
               &&
               (settings.getOutFilePatterns().isEmpty()
                ||
                !matchesAnyPattern(sf.getFileName(), settings.getOutFilePatterns()))
               &&
               (settings.getInFileTypes().isEmpty()
                ||
                settings.getInFileTypes().contains(sf.getFileType()))
               &&
               (settings.getOutFileTypes().isEmpty()
                ||
                !settings.getOutFileTypes().contains(sf.getFileType()));
    }

    boolean isArchiveFindFile(final File f) {
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
            return settings.getIncludeArchives() && isArchiveFindFile(f);
        }
        return !settings.getArchivesOnly() && isFindFile(f);
    }

    FindFile filterToFindFile(final File f) {
        if (FileUtil.isHidden(f) && settings.getExcludeHidden()) {
            return null;
        }
        FileType fileType = fileTypes.getFileType(f);
        FindFile findFile = new FindFile(f.getParent(), f.getName(), fileType);
        if (fileTypes.getFileType(f) == FileType.ARCHIVE) {
            if ((settings.getIncludeArchives() || settings.getArchivesOnly())
                && isArchiveFindFile(f)) {
                    return findFile;
            }
            return null;
        }
        if (!settings.getArchivesOnly() && isFindFile(findFile)) {
            return findFile;
        }
        return null;
    }

    public final List<FindFile> find() throws FindException {
        List<FindFile> findFiles = new ArrayList<>();
        for (String p : settings.getPaths()) {
            File pFile = new File(p);
            if (pFile.isDirectory()) {
                if (isFindDir(pFile)) {
                    findFiles.addAll(findPath(pFile));
                } else {
                    throw new FindException("Startpath does not match find settings");
                }
            } else if (pFile.isFile()) {
                FindFile findFile = filterToFindFile(pFile);
                if (findFile != null) {
                    findFiles.add(findFile);
                } else {
                    throw new FindException("Startpath does not match find settings");
                }
            } else {
                throw new FindException("Startpath is not a findable file type");
            }
        }
        return findFiles;
    }

    private static class FindFindFileVisitor extends SimpleFileVisitor<Path> {
        Function<File, Boolean> filterDir;
        Function<File, FindFile> filterToFindFile;
        List<FindFile> findFiles;

        FindFindFileVisitor(final Function<File, Boolean> filterDir,
                            final Function<File, FindFile> filterToFindFile) {
            super();
            this.filterDir = filterDir;
            this.filterToFindFile = filterToFindFile;
            findFiles = new ArrayList<>();
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
                FindFile ff = filterToFindFile.apply(f);
                if (ff != null) {
                    findFiles.add(ff);
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

    private List<FindFile> findPath(final File filePath) {
        Path startPath = filePath.toPath();
        FindFindFileVisitor findFindFileVisitor = new FindFindFileVisitor(this::isFindDir,
                this::filterToFindFile);

        // walk file tree to find files
        try {
            Files.walkFileTree(startPath, findFindFileVisitor);
        } catch (IOException e) {
            e.printStackTrace();
        }

        List<FindFile> findFiles = findFindFileVisitor.findFiles;
        return findFiles;
    }
}
