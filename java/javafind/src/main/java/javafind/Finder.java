/*******************************************************************************
Finder

Class to perform the file find

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

import org.apache.commons.io.LineIterator;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Finder {

    final private FindSettings settings;
    final private FileTypes fileTypes;
    private Charset charset;

    public Finder(final FindSettings settings) {
        this.settings = settings;
        this.fileTypes = new FileTypes();
    }

    private void log(final String message) {
        System.out.println(message);
    }

    final void validateSettings() throws FindException {
        String startPath = settings.getStartPath();
        if (null == startPath || startPath.isEmpty()) {
            throw new FindException("Startpath not defined");
        }
        File startPathFile = new File(startPath);
        if (!startPathFile.exists()) {
            throw new FindException("Startpath not found");
        }
        if (!startPathFile.canRead()) {
            throw new FindException("Startpath not readable");
        }
        if (settings.getFindPatterns().isEmpty()) {
            throw new FindException("No find patterns defined");
        }
        if (settings.getLinesAfter() < 0) {
            throw new FindException("Invalid linesafter");
        }
        if (settings.getLinesBefore() < 0) {
            throw new FindException("Invalid linesbefore");
        }
        if (settings.getMaxLineLength() < 0) {
            throw new FindException("Invalid maxlinelength");
        }
        try {
            charset = Charset.forName(settings.getTextFileEncoding());
        } catch (IllegalArgumentException e) {
            throw new FindException("Invalid encoding provided");
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
                settings.getOutFileTypes().contains(sf.getFileType()));
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
            return settings.getFindArchives() && isArchiveFindFile(f);
        }
        return !settings.getArchivesOnly() && isFindFile(f);
    }

    FindFile filterToFindFile(final File f) {
        if (FileUtil.isHidden(f) && settings.getExcludeHidden()) {
            return null;
        }
        FileType fileType = fileTypes.getFileType(f);
        FindFile findFile = new FindFile(f.getParent(), f.getName(), fileType);
        if ((fileType == FileType.ARCHIVE
                && (settings.getFindArchives() || settings.getArchivesOnly())
                && isArchiveFindFile(f))
                || (!settings.getArchivesOnly() && isFindFile(findFile))) {
            return findFile;
        }
        return null;
    }

    public final List<FindResult> find() throws FindException {
        // figure out if startPath is a directory or a file and find accordingly
        List<FindResult> results = new ArrayList<>();
        File startPathFile = new File(settings.getStartPath());
        if (startPathFile.isDirectory()) {
            if (isFindDir(startPathFile)) {
                results.addAll(findPath(startPathFile));
            } else {
                throw new FindException("Startpath does not match find settings");
            }
        } else if (startPathFile.isFile()) {
            FindFile findFile = filterToFindFile(startPathFile);
            if (findFile != null) {
                results.addAll(findFile(findFile));
            } else {
                throw new FindException("Startpath does not match find settings");
            }
        } else {
            throw new FindException("Startpath is not a findable file type");
        }
        if (settings.getVerbose()) {
            log("\nFile find complete.\n");
        }
        return results;
    }

    private static class FindFindFileVisitor extends SimpleFileVisitor<Path> {
        FileTypes fileTypes;
        Function<File, Boolean> filterDir;
        Function<File, FindFile> filterToFindFile;
        List<FindFile> findFiles;

        FindFindFileVisitor(FileTypes fileTypes, Function<File, Boolean> filterDir,
                              Function<File, FindFile> filterToFindFile) {
            super();
            this.fileTypes = fileTypes;
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
                FindFile sf = filterToFindFile.apply(f);
                if (sf != null) {
                    findFiles.add(sf);
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

    private List<FindResult> findPath(final File filePath) {
        Path startPath = filePath.toPath();
        FindFindFileVisitor findFindFileVisitor = new FindFindFileVisitor(fileTypes,
                this::isFindDir, this::filterToFindFile);

        // walk file tree to get find files
        try {
            Files.walkFileTree(startPath, findFindFileVisitor);
        } catch (IOException e) {
            e.printStackTrace();
        }

        List<FindFile> findFiles = findFindFileVisitor.findFiles;

        if (settings.getVerbose()) {
            List<String> findDirs = findFiles.stream()
                    .map(FindFile::getPath)
                    .distinct()
                    .sorted(String::compareTo)
                    .collect(Collectors.toList());
            log(String.format("\nDirectories to be found (%d):",
                    findDirs.size()));
            for (String d : findDirs) {
                log(d);
            }
            log(String.format("\n\nFiles to be found (%d):",
                    findFiles.size()));
            for (FindFile sf : findFiles) {
                log(sf.toString());
            }
            log("");
        }

        // find the files
        return findFiles(findFiles);
    }

    private List<FindResult> findFiles(List<FindFile> findFiles) {
        List<FindResult> results = new ArrayList<>();

        int offset = 0;
        int batchSize = 1000;

        if (findFiles.size() > batchSize) {
            do {
                int toIndex = Math.min(offset + batchSize, findFiles.size());
                List<FindFile> nextBatch = findFiles.subList(offset, toIndex);
                results.addAll(batchFindFiles(nextBatch));
                offset += batchSize;
            } while (offset < findFiles.size());
        } else {
            for (FindFile sf : findFiles) {
                results.addAll(findFile(sf));
            }
        }

        return results;
    }

    private List<FindResult> batchFindFiles(List<FindFile> findFiles) {
        //System.out.println("Next batch: " + findFiles.size() + " findFiles");
        List<FindResult> results = new ArrayList<>();

        List<CompletableFuture<List<FindResult>>> futures = new ArrayList<>(findFiles.size());
        for (FindFile sf : findFiles) {
            futures.add(CompletableFuture.supplyAsync(() -> findFile(sf)));
        }
        for (CompletableFuture<List<FindResult>> future : futures) {
            try {
                results.addAll(future.get());
            } catch (InterruptedException | ExecutionException e) {
                log(e.toString());
            }
        }
        return results;
    }

    public final List<FindResult> findFile(final FindFile sf) {
        FileType fileType = sf.getFileType();
        if (fileType == FileType.CODE || fileType == FileType.TEXT || fileType == FileType.XML) {
            return findTextFile(sf);
        } else if (fileType == FileType.BINARY) {
            return findBinaryFile(sf);
        }
        return new ArrayList<>();
    }

    private List<FindResult> findTextFile(final FindFile sf) {
        if (settings.getVerbose()) {
            log(String.format("Finding text file %s", sf.toString()));
        }
        if (settings.getMultiLineFind()) {
            return findTextFileContents(sf);
        } else {
            return findTextFileLines(sf);
        }
    }

    private List<FindResult> findTextFileContents(final FindFile sf) {
        List<FindResult> results = new ArrayList<>();
        try {
            String contents = FileUtil.getFileContents(sf.toFile(), charset);
            results.addAll(findMultiLineString(contents));
            for (FindResult r : results) {
                r.setFindFile(sf);
            }
        } catch (NoSuchElementException e) {
            log(e.toString() + ": " + sf.toString());
        } catch (IllegalStateException | IOException e) {
            log(e.toString());
        }
        return results;
    }

    private List<Number> getNewLineIndices(final String s) {
        List<Number> newlineIndices = new ArrayList<>();
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) == '\n') {
                newlineIndices.add(i);
            }
        }
        return newlineIndices;
    }

    private List<Number> getStartLineIndicesFromNewLineIndices(final List<Number> newLineIndices) {
        List<Number> startLineIndices = new ArrayList<>();
        startLineIndices.add(0);
        startLineIndices.addAll(newLineIndices.stream()
                .map(i -> i.longValue() + 1).collect(Collectors.toList()));
        return startLineIndices;
    }

    private List<Number> getEndLineIndicesFromNewLineIndices(final String s,
                                                             final List<Number> newLineIndices) {
        List<Number> endLineIndices = new ArrayList<>(newLineIndices);
        endLineIndices.add(s.length() - 1);
        return endLineIndices;
    }

    public final List<FindResult> findMultiLineString(final String s) {
        List<FindResult> results = new ArrayList<>();
        for (Pattern p : settings.getFindPatterns()) {
            results.addAll(findMultiLineStringForPattern(s, p));
        }
        return results;
    }

    private List<String> getLinesFromMultiLineString(final String s,
                                                     final List<Number> startIndices,
                                                     final List<Number> endIndices) {
        List<String> lines = new ArrayList<>();
        for (int i = 0; i < startIndices.size(); i++) {
            lines.add(s.substring(startIndices.get(i).intValue(),
                    endIndices.get(i).intValue()));
        }
        return lines;
    }

    private List<FindResult> findMultiLineStringForPattern(final String s,
                                                               final Pattern p) {
        Map<Pattern, Integer> patternMatches = new HashMap<>();
        List<FindResult> results = new ArrayList<>();
        List<Number> newLineIndices = getNewLineIndices(s);
        List<Number> startLineIndices = getStartLineIndicesFromNewLineIndices(newLineIndices);
        List<Number> endLineIndices = getEndLineIndicesFromNewLineIndices(s,
                newLineIndices);
        final int linesBeforeCount = settings.getLinesBefore();
        final int linesAfterCount = settings.getLinesAfter();
        final int sLength = s.length();
        Matcher m = p.matcher(s);
        boolean found = m.find();
        while (found) {
            if (settings.getFirstMatch() && patternMatches.containsKey(p)) {
                found = false;
            } else {
                // get the start line indices before the match index to get the current line number
                List<Number> beforeStartIndices = startLineIndices.stream()
                        .filter(i -> i.intValue() <= m.start())
                        .collect(Collectors.toList());
                int lineNum = beforeStartIndices.size();
                int nextEndLineIndex = endLineIndices.get(beforeStartIndices.size() - 1).intValue();
                if (nextEndLineIndex == sLength - 1) {
                    nextEndLineIndex++;
                }
                int endLineIndex = nextEndLineIndex;
                int startLineIndex = beforeStartIndices
                        .remove(beforeStartIndices.size() - 1).intValue();
                String line = s.substring(startLineIndex, endLineIndex);

                List<String> linesBefore;
                if (linesBeforeCount > 0) {
                    List<Number> beforeEndIndices = endLineIndices.stream()
                            .filter(i -> i.intValue() < m.start())
                            .collect(Collectors.toList());
                    List<Number> linesBeforeStartIndices = ListUtil
                            .takeRight(beforeStartIndices, linesBeforeCount);
                    List<Number> linesBeforeEndIndices = ListUtil
                            .takeRight(beforeEndIndices, linesBeforeCount);
                    linesBefore = getLinesFromMultiLineString(s,
                            linesBeforeStartIndices, linesBeforeEndIndices);
                } else {
                    linesBefore = new ArrayList<>();
                }

                List<String> linesAfter;
                if (linesAfterCount > 0) {
                    List<Number> afterStartIndices = startLineIndices.stream()
                            .filter(i -> i.intValue() > m.start())
                            .limit(linesAfterCount)
                            .collect(Collectors.toList());
                    List<Number> afterEndIndices = endLineIndices.stream()
                            .filter(i -> i.intValue() > endLineIndex)
                            .limit(linesAfterCount)
                            .collect(Collectors.toList());
                    linesAfter = getLinesFromMultiLineString(s,
                            afterStartIndices, afterEndIndices);
                } else {
                    linesAfter = new ArrayList<>();
                }

                if ((linesBefore.isEmpty() || linesBeforeMatch(linesBefore))
                        &&
                        (linesAfter.isEmpty() || linesAfterMatch(linesAfter))) {
                    FindResult findResult = new FindResult(
                            p,
                            null,
                            lineNum,
                            m.start() - startLineIndex + 1,
                            m.end() - startLineIndex + 1,
                            line,
                            linesBefore,
                            linesAfter);
                    results.add(findResult);
                    patternMatches.put(p, 1);
                }
                found = m.find(m.end());
            }
        }
        return results;
    }

    private List<FindResult> findTextFileLines(final FindFile sf) {
        LineIterator it = null;
        List<FindResult> results = new ArrayList<>();
        try {
            it = FileUtil.getFileLineIterator(sf.toFile(), settings.getTextFileEncoding());
            results.addAll(findStringIterator(it));
            for (FindResult r : results) {
                r.setFindFile(sf);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (it != null) {
                try {
                    it.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        return results;
    }

    private boolean linesMatch(final List<String> lines,
                               final Set<Pattern> inPatterns,
                               final Set<Pattern> outPatterns) {
        return ((inPatterns.size() == 0 || anyMatchesAnyPattern(lines, inPatterns))
                &&
                (outPatterns.size() == 0 || !anyMatchesAnyPattern(lines, outPatterns)));
    }

    private boolean linesBeforeMatch(final List<String> linesBefore) {
        return linesMatch(linesBefore, settings.getInLinesBeforePatterns(),
                settings.getOutLinesBeforePatterns());
    }

    private boolean linesAfterMatch(final List<String> linesAfter) {
        return linesMatch(linesAfter, settings.getInLinesAfterPatterns(),
                settings.getOutLinesAfterPatterns());
    }

    private boolean linesAfterToOrUntilMatch(final Iterator<String> it, List<String> linesAfter) {
        Set<Pattern> linesAfterPatterns;
        if (settings.hasLinesAfterToPatterns()) {
            linesAfterPatterns = settings.getLinesAfterToPatterns();
        } else if (settings.hasLinesAfterUntilPatterns()) {
            linesAfterPatterns = settings.getLinesAfterUntilPatterns();
        } else { // should never get here
            linesAfterPatterns = new HashSet<>();
        }
        boolean linesAfterMatch = anyMatchesAnyPattern(linesAfter, linesAfterPatterns);
        while (!linesAfterMatch && it.hasNext()) {
            String nextLine = it.next();
            linesAfter.add(nextLine);
            linesAfterMatch = matchesAnyPattern(nextLine, linesAfterPatterns);
        }
        if (linesAfterMatch) {
            if (settings.hasLinesAfterUntilPatterns()) {
                linesAfter.remove(linesAfter.size() - 1);
            }
        }
        return linesAfterMatch;
    }

    public final List<FindResult> findStringIterator(final Iterator<String> it) {
        int lineNum = 0;
        String line;
        final int linesBeforeCount = settings.getLinesBefore();
        final int linesAfterCount = settings.getLinesAfter();
        List<String> linesBefore = new ArrayList<>();
        List<String> linesAfter = new ArrayList<>();
        Set<Pattern> matchedPatterns = new HashSet<>();
        List<FindResult> results = new ArrayList<>();
        while (true) {
            lineNum++;
            if (!linesAfter.isEmpty()) {
                line = linesAfter.remove(0);
            } else if (it.hasNext()) {
                line = it.next();
            } else {
                break;
            }
            if (linesAfterCount > 0) {
                while (linesAfter.size() < linesAfterCount && it.hasNext()) {
                    linesAfter.add(it.next());
                }
            }

            Set<Pattern> findPatterns = settings.getFindPatterns()
                    .stream().filter(p -> !matchedPatterns.contains(p))
                    .collect(Collectors.toSet());
            for (Pattern p : findPatterns) {
                Matcher m = p.matcher(line);
                boolean found = m.find();
                while (found) {
                    if ((linesBefore.isEmpty() || linesBeforeMatch(linesBefore))
                            &&
                            (linesAfter.isEmpty() || linesAfterMatch(linesAfter))
                            &&
                            (!settings.hasLinesAfterToOrUntilPatterns() ||
                                    linesAfterToOrUntilMatch(it, linesAfter))
                            )
                    {
                        FindResult findResult = new FindResult(
                                p,
                                null,
                                lineNum,
                                m.start() + 1,
                                m.end() + 1,
                                line,
                                new ArrayList<>(linesBefore),
                                new ArrayList<>(linesAfter));
                        results.add(findResult);
                        if (settings.getFirstMatch()) {
                            matchedPatterns.add(p);
                            found = false;
                        } else {
                            found = m.find(m.end());
                        }
                    } else {
                        found = false;
                    }
                }
            }

            if (linesBeforeCount > 0) {
                if (linesBefore.size() == linesBeforeCount) {
                    linesBefore.remove(0);
                }
                if (linesBefore.size() < linesBeforeCount) {
                    linesBefore.add(line);
                }
            }
            if (settings.getFirstMatch() &&
                    matchedPatterns.size() == settings.getFindPatterns().size()) {
                break;
            }
        }
        return results;
    }

    private List<FindResult> findBinaryFile(final FindFile sf) {
        if (settings.getVerbose()) {
            log(String.format("Finding binary file %s", sf.getPath()));
        }
        List<FindResult> results = new ArrayList<>();
        try {
            String content = FileUtil.getFileContents(sf.toFile(), StandardCharsets.ISO_8859_1);
            for (Pattern p : settings.getFindPatterns()) {
                Matcher m = p.matcher(content);
                boolean found = m.find();
                while (found) {
                    results.add(new FindResult(
                            p,
                            sf,
                            0,
                            m.start() + 1,
                            m.end() + 1,
                            ""));
                    found = !settings.getFirstMatch() && m.find(m.end());
                }
            }
        } catch (IOException | NoSuchElementException | IllegalStateException e) {
            log(e.toString());
        }
        return results;
    }
}
