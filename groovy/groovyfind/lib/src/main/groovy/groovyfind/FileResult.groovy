package groovyfind


import java.nio.file.Path
import java.nio.file.attribute.FileTime

class FileResult {

    static final String CONTAINER_SEPARATOR = '!'
    final List<String> containers
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

    FileResult(final List<String> containers, final Path path, final FileType fileType,
               final long fileSize, final FileTime lastMod) {
        this.containers = containers
        this.path = path
        this.fileType = fileType
        this.fileSize = fileSize
        this.lastMod = lastMod
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
        if (this.fileSize == other.fileSize) {
            return compareByPath(other, sortCaseInsensitive)
        }
        return this.fileSize <= other.fileSize ? -1 : 1
    }

    int compareByType(final FileResult other, final boolean sortCaseInsensitive) {
        if (this.fileType == other.fileType) {
            return compareByPath(other, sortCaseInsensitive)
        }
        return this.fileType <=> other.fileType
    }

    int compareByLastMod(final FileResult other, final boolean sortCaseInsensitive) {
        if (this.lastMod == other.lastMod) {
            return compareByPath(other, sortCaseInsensitive)
        }
        return this.lastMod <=> other.lastMod
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
