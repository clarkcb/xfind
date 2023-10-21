package ktfind

import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import java.util.*

/**
 * @author cary on 7/24/16.
 */
class FileResult(val containers: List<String>,
                 val path: Path,
                 val fileType: FileType,
                 val stat: BasicFileAttributes? = null) {
    val CONTAINER_SEPARATOR = "!"

    constructor(path: Path, fileType: FileType) : this(listOf(), path, fileType)
    constructor(path: Path, fileType: FileType, stat: BasicFileAttributes?) : this(listOf(), path, fileType, stat)

    private fun compareStrings(str1: String, str2: String, sortCaseInsensitive: Boolean): Int {
        val s1 =
            if (sortCaseInsensitive) str1.lowercase(Locale.getDefault())
            else str1
        val s2 =
            if (sortCaseInsensitive) str2.lowercase(Locale.getDefault())
            else str2
        return s1.compareTo(s2)
    }

    fun compareByPath(other: FileResult, sortCaseInsensitive: Boolean): Int {
        val pathCmp = compareStrings(path.parent.toString(), other.path.parent.toString(), sortCaseInsensitive)
        return if (pathCmp == 0) {
            compareStrings(path.fileName.toString(), other.path.fileName.toString(), sortCaseInsensitive)
        } else pathCmp
    }

    fun compareByName(other: FileResult, sortCaseInsensitive: Boolean): Int {
        val nameCmp = compareStrings(path.fileName.toString(), other.path.fileName.toString(), sortCaseInsensitive)
        return if (nameCmp == 0) {
            compareStrings(path.parent.toString(), other.path.parent.toString(), sortCaseInsensitive)
        } else nameCmp
    }

    fun compareBySize(other: FileResult, sortCaseInsensitive: Boolean): Int {
        if (stat == null || other.stat == null) return 0
        val sizeCmp = stat.size().compareTo(other.stat.size())
        return if (sizeCmp == 0) {
            compareByPath(other, sortCaseInsensitive)
        } else sizeCmp
    }

    fun compareByType(other: FileResult, sortCaseInsensitive: Boolean): Int {
        val fileTypeCmp = fileType.compareTo(other.fileType)
        return if (fileTypeCmp == 0) {
            compareByPath(other, sortCaseInsensitive)
        } else fileTypeCmp
    }

    fun compareByLastMod(other: FileResult, sortCaseInsensitive: Boolean): Int {
        if (stat == null || other.stat == null) return 0
        return if (stat.lastModifiedTime() == other.stat.lastModifiedTime()) {
            compareByPath(other, sortCaseInsensitive)
        } else stat.lastModifiedTime().compareTo(other.stat.lastModifiedTime())
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
