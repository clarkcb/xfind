package ktfind

// TODO: switch to using kotlin.io.path?
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.attribute.FileTime
import java.util.*

/**
 * @author cary on 7/24/16.
 */
class FileResult(
    val containers: List<Path>,
    val path: Path,
    val fileType: FileType,
    val fileSize: Long,
    val lastMod: FileTime? = null
) : Comparable<FileResult> {
    companion object {
        const val CONTAINER_SEPARATOR = "!"
    }

    constructor(path: Path, fileType: FileType) : this(listOf(), path, fileType, 0)
    constructor(path: Path, fileType: FileType, fileSize: Long, lastMod: FileTime?) :
            this(listOf(), path, fileType, fileSize, lastMod)

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
        val sizeCmp = fileSize.compareTo(other.fileSize)
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
        if (lastMod == null && other.lastMod == null) return 0
        if (lastMod == null) return 1
        if (other.lastMod == null) return -1
        val lastModCmp = lastMod.compareTo(other.lastMod)
        return if (lastModCmp == 0) {
            compareByPath(other, sortCaseInsensitive)
        } else lastModCmp
    }

    override fun toString(): String {
        val sb = StringBuilder()
        if (containers.isNotEmpty()) {
            for (i in containers.indices) {
                if (i > 0) {
                    sb.append(CONTAINER_SEPARATOR)
                }
                sb.append(containers[i].toString())
            }
            sb.append(CONTAINER_SEPARATOR)
        }
        sb.append(path)
        return sb.toString()
    }

    override fun compareTo(other: FileResult): Int {
        val cmpPath = compareByPath(other, false)
        if (cmpPath != 0) return cmpPath
        return compareByName(other, false)
    }
}

class FileResultFormatter(val settings: FindSettings) {

    fun colorize(s: String, matchStartIndex: Int, matchEndIndex: Int, color: Color): String {
        val prefix = if (matchStartIndex > 0) s.substring(0, matchStartIndex) else ""
        val suffix = if (matchEndIndex < s.length) s.substring(matchEndIndex) else ""
        return prefix +
                color.toConsoleColor().value +
                s.substring(matchStartIndex, matchEndIndex) +
                ConsoleColor.RESET.value +
                suffix
    }

    private fun formatDirPathWithColor(dirPath: Path): String {
        var formattedDirPath = dirPath.toString()
        for (p in settings.inDirPatterns) {
            val m = p.find(formattedDirPath)
            if (m != null) {
                formattedDirPath = colorize(formattedDirPath, m.range.first, m.range.last + 1, settings.dirColor)
                break
            }
        }
        return formattedDirPath
    }

    val formatDirPath = if (settings.colorize && !settings.inDirPatterns.isEmpty()) {
        this::formatDirPathWithColor
    } else {
        { dirPath: Path -> dirPath.toString() }
    }

    private fun formatFileNameWithColor(fileName: String): String {
        var formattedFileName = fileName
        for (p in settings.inFilePatterns) {
            val m = p.find(formattedFileName)
            if (m != null) {
                formattedFileName = colorize(formattedFileName, m.range.first, m.range.last + 1, settings.fileColor)
                break
            }
        }
        if (!settings.inExtensions.isEmpty()) {
            val idx = formattedFileName.lastIndexOf('.')
            if (idx > 0 && idx < formattedFileName.length - 1) {
                formattedFileName = colorize(formattedFileName, idx + 1, formattedFileName.length, settings.extColor)
            }
        }
        return formattedFileName
    }

    val formatFileName = if (settings.colorize && (!settings.inFilePatterns.isEmpty() || !settings.inExtensions.isEmpty())) {
        this::formatFileNameWithColor
    } else {
        { fileName: String -> fileName }
    }

    fun formatPath(path: Path): String {
        var parent = "."
        if (path.parent != null) {
            parent = formatDirPath(path.parent)
        }
        val fileName = formatFileName(path.fileName.toString())

        return Paths.get(parent, fileName).toString()
    }

    fun formatFileResult(result: FileResult): String {
        return formatPath(result.path)
    }
}


class FileResultSorter(val settings: FindSettings) {
    private fun getFileResultComparator(): Comparator<FileResult> {
        return if (settings.sortDescending) {
            when (settings.sortBy) {
                SortBy.FILENAME -> Comparator { fr1, fr2 ->
                    fr2.compareByName(
                        fr1,
                        settings.sortCaseInsensitive
                    )
                }
                SortBy.FILESIZE -> Comparator { fr1, fr2 ->
                    fr2.compareBySize(
                        fr1,
                        settings.sortCaseInsensitive
                    )
                }
                SortBy.FILETYPE -> Comparator { fr1, fr2 ->
                    fr2.compareByType(
                        fr1,
                        settings.sortCaseInsensitive
                    )
                }
                SortBy.LASTMOD -> Comparator { fr1, fr2 ->
                    fr2.compareByLastMod(
                        fr1,
                        settings.sortCaseInsensitive
                    )
                }
                else -> Comparator { fr1, fr2 -> fr2.compareByPath(fr1, settings.sortCaseInsensitive) }
            }
        } else {
            when (settings.sortBy) {
                SortBy.FILENAME -> Comparator { fr1, fr2 ->
                    fr1.compareByName(
                        fr2,
                        settings.sortCaseInsensitive
                    )
                }
                SortBy.FILESIZE -> Comparator { fr1, fr2 ->
                    fr1.compareBySize(
                        fr2,
                        settings.sortCaseInsensitive
                    )
                }
                SortBy.FILETYPE -> Comparator { fr1, fr2 ->
                    fr1.compareByType(
                        fr2,
                        settings.sortCaseInsensitive
                    )
                }
                SortBy.LASTMOD -> Comparator { fr1, fr2 ->
                    fr1.compareByLastMod(
                        fr2,
                        settings.sortCaseInsensitive
                    )
                }
                else -> Comparator { fr1, fr2 -> fr1.compareByPath(fr2, settings.sortCaseInsensitive) }
            }
        }
    }

    fun sort(fileResults: List<FileResult>): List<FileResult> {
        if (fileResults.isEmpty()) {
            return emptyList()
        }
        val fileResultsComparator = getFileResultComparator()
        return fileResults.stream().sorted(fileResultsComparator).toList()
    }
}

