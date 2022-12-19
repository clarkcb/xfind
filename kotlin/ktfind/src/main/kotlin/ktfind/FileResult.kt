package ktfind

import java.nio.file.Path

/**
 * @author cary on 7/24/16.
 */
class FileResult(val containers: List<String>, val path: Path, val fileType: FileType) {
    val CONTAINER_SEPARATOR = "!"

    constructor(path: Path, fileType: FileType) : this(listOf(), path, fileType)

    fun compareByPath(other: FileResult): Int {
        return if (path.parent == other.path.parent) {
            path.fileName.compareTo(other.path.fileName)
        } else path.parent.compareTo(other.path.parent)
    }

    fun compareByName(other: FileResult): Int {
        return if (path.fileName == other.path.fileName) {
            path.parent.compareTo(other.path.parent)
        } else path.fileName.compareTo(other.path.fileName)
    }

    fun compareByType(other: FileResult): Int {
        return if (fileType == other.fileType) {
            compareByPath(other)
        } else fileType.compareTo(other.fileType)
    }

    override fun toString(): String {
        val sb = StringBuilder()
        if (containers.isNotEmpty()) {
            for (i in containers.indices) {
                if (i > 0) {
                    sb.append(CONTAINER_SEPARATOR)
                }
                sb.append(containers[i])
            }
            sb.append(CONTAINER_SEPARATOR)
        }
        sb.append(path)
        return sb.toString()
    }
}
