package groovyfind


import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes

class FileResult {

    static final String CONTAINER_SEPARATOR = '!'
    final List<String> containers
    final Path path
    final FileType fileType
    final BasicFileAttributes stat

    FileResult(final Path path, final FileType fileType) {
        this([], path, fileType, null)
    }

    FileResult(final Path path, final FileType fileType, final BasicFileAttributes stat) {
        this([], path, fileType, stat)
    }

    FileResult(final List<String> containers, final Path path, final FileType fileType,
               final BasicFileAttributes stat) {
        this.containers = containers
        this.path = path
        this.fileType = fileType
        this.stat = stat
    }

    static int compareElements(final List firstElems, final List secondElems) {
        int i = 0
        while (i < firstElems.size() && i < secondElems.size()) {
            Object firstElem = firstElems.get(i)
            Object secondElem = secondElems.get(i)
            if (firstElem != secondElem) {
                return firstElem <=> secondElem
            }
            i++
        }
        return 0
    }

    int compareByPath(final FileResult other, final boolean sortCaseInsensitive) {
        if (sortCaseInsensitive) {
            return compareElements(
                    [this.path.parent.toString().toLowerCase(), this.path.fileName.toString().toLowerCase()],
                    [other.path.parent.toString().toLowerCase(), other.path.fileName.toString().toLowerCase()]
            )
        }
        return compareElements(
                [this.path.parent.toString(), this.path.fileName.toString()],
                [other.path.parent.toString(), other.path.fileName.toString()]
        )
    }

    int compareByName(final FileResult other, final boolean sortCaseInsensitive) {
        if (sortCaseInsensitive) {
            return compareElements(
                    [this.path.fileName.toString().toLowerCase(), this.path.parent.toString().toLowerCase()],
                    [other.path.fileName.toString().toLowerCase(), other.path.parent.toString().toLowerCase()]
            )
        }
        return compareElements(
                [this.path.fileName.toString(), this.path.parent.toString()],
                [other.path.fileName.toString(), other.path.parent.toString()]
        )
    }

    int compareBySize(final FileResult other, final boolean sortCaseInsensitive) {
        if (this.stat != null && other.stat != null) {
            if (this.stat.size() == other.stat.size()) {
                return compareByPath(other, sortCaseInsensitive)
            }
            return this.stat.size() <= other.stat.size() ? -1 : 1
        }
        return 0
    }

    int compareByType(final FileResult other, final boolean sortCaseInsensitive) {
        if (this.fileType == other.fileType) {
            return compareByPath(other, sortCaseInsensitive)
        }
        return this.fileType <=> other.fileType
    }

    int compareByLastMod(final FileResult other, final boolean sortCaseInsensitive) {
        if (this.stat != null && other.stat != null) {
            if (this.stat.lastModifiedTime() == other.stat.lastModifiedTime()) {
                return compareByPath(other, sortCaseInsensitive)
            }
            return this.stat.lastModifiedTime() <=> other.stat.lastModifiedTime()
        }
        return 0
    }

    String toString() {
        StringBuilder sb = new StringBuilder()
        if (!containers.empty) {
            for (int i = 0; i < containers.size(); i++) {
                if (i > 0) {
                    sb.append(CONTAINER_SEPARATOR)
                }
                sb.append(containers.get(i))
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
