package groovyfind


import java.nio.file.Path
import java.nio.file.attribute.FileTime
import java.util.function.Function
import java.util.regex.Matcher

class FileResult {

    static final String CONTAINER_SEPARATOR = '!'
    final List<Path> containers
    final Path path
    final FileType fileType
    final long fileSize
    final FileTime lastMod

    FileResult(final Path path, final FileType fileType) {
        this([], path, fileType, 0L, null)
    }

    FileResult(final Path path, final FileType fileType, final long fileSize, final FileTime lastMod) {
        this([], path, fileType, fileSize, lastMod)
    }

    FileResult(final List<Path> containers, final Path path, final FileType fileType,
               final long fileSize, final FileTime lastMod) {
        this.containers = containers
        this.path = path
        this.fileType = fileType
        this.fileSize = fileSize
        this.lastMod = lastMod
    }

    private static int compareStrings(final String first, final String second, final boolean sortCaseInsensitive) {
        if (sortCaseInsensitive) {
            first.toLowerCase() <=> second.toLowerCase()
        } else {
            first <=> second
        }
    }

    int compareByPath(final FileResult other, final boolean sortCaseInsensitive) {
        int pComp = compareStrings(this.path.parent.toString(), other.path.parent.toString(), sortCaseInsensitive)
        if (pComp == 0) {
            compareStrings(this.path.fileName.toString(), other.path.fileName.toString(), sortCaseInsensitive)
        } else {
            pComp
        }
    }

    int compareByName(final FileResult other, final boolean sortCaseInsensitive) {
        int fComp = compareStrings(this.path.fileName.toString(), other.path.fileName.toString(), sortCaseInsensitive)
        if (fComp == 0) {
            compareStrings(this.path.parent.toString(), other.path.parent.toString(), sortCaseInsensitive)
        } else {
            fComp
        }
    }

    int compareBySize(final FileResult other, final boolean sortCaseInsensitive) {
        int fsComp = this.fileSize <=> other.fileSize
        if (fsComp == 0) {
            compareByPath(other, sortCaseInsensitive)
        } else {
            fsComp
        }
    }

    int compareByType(final FileResult other, final boolean sortCaseInsensitive) {
        int ftComp = this.fileType <=> other.fileType
        if (ftComp == 0) {
            compareByPath(other, sortCaseInsensitive)
        } else {
            ftComp
        }
    }

    int compareByLastMod(final FileResult other, final boolean sortCaseInsensitive) {
        if (lastMod == null && other.lastMod == null) return 0
        if (lastMod == null) return 1
        if (other.lastMod == null) return -1
        int lmComp = this.lastMod <=> other.lastMod
        if (lmComp == 0) {
            compareByPath(other, sortCaseInsensitive)
        } else {
            lmComp
        }
    }

    String toString() {
        StringBuilder sb = new StringBuilder()
        if (!containers.empty) {
            for (int i = 0; i < containers.size(); i++) {
                if (i > 0) {
                    sb.append(CONTAINER_SEPARATOR)
                }
                sb.append(containers.get(i).toString())
            }
            sb.append(CONTAINER_SEPARATOR)
        }
        sb.append(path.toString())
        sb.toString()
    }
}

class FileResultFormatter {

    private final FindSettings settings
    private final Function<Path, String> formatDirPathFunc
    private final Function<String, String> formatFileNameFunc

    FileResultFormatter(final FindSettings settings) {
        this.settings = settings
        if (settings.colorize && !settings.inDirPatterns.isEmpty()) {
            this.formatDirPathFunc = this.&formatDirPathWithColor
        } else {
            this.formatDirPathFunc = Path::toString
        }
        if (settings.colorize && (!settings.inFilePatterns.isEmpty() || !settings.inExtensions.isEmpty())) {
            this.formatFileNameFunc = this.&formatFileNameWithColor
        } else {
            this.formatFileNameFunc = String::toString
        }
    }

    String colorize(final String s, final int matchStartIndex, final int matchEndIndex) {
        String prefix = ''
        if (matchStartIndex > 0) {
            prefix = s.substring(0, matchStartIndex)
        }
        String suffix = ''
        if (matchEndIndex < s.length()) {
            suffix = s.substring(matchEndIndex)
        }
        return prefix +
                Color.GREEN +
                s.substring(matchStartIndex, matchEndIndex) +
                Color.RESET +
                suffix;
    }

    private String formatDirPathWithColor(final Path dirPath) {
        String formattedDirPath = dirPath.toString()
        for (var p : settings.getInDirPatterns()) {
            Matcher m = p.matcher(formattedDirPath)
            if (m.find()) {
                formattedDirPath = colorize(formattedDirPath, m.start(), m.end())
                break
            }
        }
        return formattedDirPath
    }

    String formatDirPath(final Path dirPath) {
        formatDirPathFunc.apply(dirPath)
    }

    private String formatFileNameWithColor(final String fileName) {
        String formattedFileName = fileName
        for (var p : settings.getInFilePatterns()) {
            Matcher m = p.matcher(formattedFileName)
            if (m.find()) {
                formattedFileName = colorize(formattedFileName, m.start(), m.end())
                break
            }
        }
        if (!settings.getInExtensions().isEmpty()) {
            var idx = formattedFileName.lastIndexOf('.')
            if (idx > 0 && idx < formattedFileName.length() - 1) {
                formattedFileName = colorize(formattedFileName, idx + 1, formattedFileName.length())
            }
        }
        return formattedFileName
    }

    String formatFileName(final String fileName) {
        formatFileNameFunc.apply(fileName)
    }

    String formatPath(final Path path) {
        String parent = '.'
        if (path.getParent() != null) {
            parent = formatDirPath(path.getParent())
        }
        String fileName = formatFileName(path.fileName.toString())
        return Path.of(parent, fileName).toString()
    }

    String formatFileResult(final FileResult result) {
        formatPath(result.path)
    }
}

class FileResultSorter {

    private final FindSettings settings

    FileResultSorter(final FindSettings settings) {
        this.settings = settings
    }

    final Comparator<FileResult> getFileResultComparator() {
        if (settings.sortDescending) {
            switch (settings.sortBy) {
                case SortBy.FILENAME:
                    return (FileResult fr1, FileResult fr2) -> fr2.compareByName(fr1, settings.sortCaseInsensitive)
                case SortBy.FILESIZE:
                    return (FileResult fr1, FileResult fr2) -> fr2.compareBySize(fr1, settings.sortCaseInsensitive)
                case SortBy.FILETYPE:
                    return (FileResult fr1, FileResult fr2) -> fr2.compareByType(fr1, settings.sortCaseInsensitive)
                case SortBy.LASTMOD:
                    return (FileResult fr1, FileResult fr2) -> fr2.compareByLastMod(fr1, settings.sortCaseInsensitive)
                default:
                    return (FileResult fr1, FileResult fr2) -> fr2.compareByPath(fr1, settings.sortCaseInsensitive)
            }
        }
        switch (settings.sortBy) {
            case SortBy.FILENAME:
                return (FileResult fr1, FileResult fr2) -> fr1.compareByName(fr2, settings.sortCaseInsensitive)
            case SortBy.FILESIZE:
                return (FileResult fr1, FileResult fr2) -> fr1.compareBySize(fr2, settings.sortCaseInsensitive)
            case SortBy.FILETYPE:
                return (FileResult fr1, FileResult fr2) -> fr1.compareByType(fr2, settings.sortCaseInsensitive)
            case SortBy.LASTMOD:
                return (FileResult fr1, FileResult fr2) -> fr1.compareByLastMod(fr2, settings.sortCaseInsensitive)
            default:
                return (FileResult fr1, FileResult fr2) -> fr1.compareByPath(fr2, settings.sortCaseInsensitive)
        }
    }

    final void sort(List<FileResult> fileResults) {
        if (fileResults.empty) {
            return
        }
        def comparator = getFileResultComparator()
        fileResults.sort(comparator)
    }
}
