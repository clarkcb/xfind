package groovyfind


import java.nio.file.Path
import java.nio.file.attribute.FileTime

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
            return first.toLowerCase() <=> second.toLowerCase()
        }
        return first <=> second
    }

    int compareByPath(final FileResult other, final boolean sortCaseInsensitive) {
        int pComp = compareStrings(this.path.parent.toString(), other.path.parent.toString(), sortCaseInsensitive)
        if (pComp == 0) {
            return compareStrings(this.path.fileName.toString(), other.path.fileName.toString(), sortCaseInsensitive)
        }
        return pComp
    }

    int compareByName(final FileResult other, final boolean sortCaseInsensitive) {
        int fComp = compareStrings(this.path.fileName.toString(), other.path.fileName.toString(), sortCaseInsensitive)
        if (fComp == 0) {
            return compareStrings(this.path.parent.toString(), other.path.parent.toString(), sortCaseInsensitive)
        }
        return fComp
    }

    int compareBySize(final FileResult other, final boolean sortCaseInsensitive) {
        int fsComp = this.fileSize <=> other.fileSize
        if (fsComp == 0) {
            return compareByPath(other, sortCaseInsensitive)
        }
        return fsComp
    }

    int compareByType(final FileResult other, final boolean sortCaseInsensitive) {
        int ftComp = this.fileType <=> other.fileType
        if (ftComp == 0) {
            return compareByPath(other, sortCaseInsensitive)
        }
        return ftComp
    }

    int compareByLastMod(final FileResult other, final boolean sortCaseInsensitive) {
        if (lastMod == null && other.lastMod == null) return 0
        if (lastMod == null) return 1
        if (other.lastMod == null) return -1
        int lmComp = this.lastMod <=> other.lastMod
        if (lmComp == 0) {
            return compareByPath(other, sortCaseInsensitive)
        }
        return lmComp
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
//        if (stat != null) {
//            sb.append(" (").append(stat.lastModifiedTime().toInstant().getEpochSecond()).append(")");
//        }
        return sb.toString()
    }

}
