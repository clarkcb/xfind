package ktfind

import java.nio.file.Path
import java.nio.file.Paths
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException

/**
 * @author cary on 7/23/16.
 */
enum class SortBy(val value: String) {
    FILEPATH("filepath"),
    FILENAME("filename"),
    FILESIZE("filesize"),
    FILETYPE("filetype"),
    LASTMOD("lastmod");

    override fun toString(): String {
        return value
    }

    companion object {
        fun forName(name: String): SortBy {
            val lname = name.trim().lowercase()
            entries.forEach {
                if (it.value == lname) {
                    return it
                }
            }
            if (lname == "name") {
                return FILENAME
            }
            if (lname == "size") {
                return FILESIZE
            }
            if (lname == "type") {
                return FILETYPE
            }
            return FILEPATH
        }
    }
}

data class FindSettings(
    val archivesOnly: Boolean,
    val debug: Boolean,
    val inArchiveExtensions: Set<String>,
    val inArchiveFilePatterns: Set<Regex>,
    val inDirPatterns: Set<Regex>,
    val inExtensions: Set<String>,
    val inFilePatterns: Set<Regex>,
    val inFileTypes: Set<FileType>,
    val includeArchives: Boolean,
    val includeHidden: Boolean,
    val maxDepth: Int,
    val maxLastMod: LocalDateTime?,
    val maxSize: Int,
    val minDepth: Int,
    val minLastMod: LocalDateTime?,
    val minSize: Int,
    val outArchiveExtensions: Set<String>,
    val outArchiveFilePatterns: Set<Regex>,
    val outDirPatterns: Set<Regex>,
    val outExtensions: Set<String>,
    val outFilePatterns: Set<Regex>,
    val outFileTypes: Set<FileType>,
    val paths: Set<String>,
    val printDirs: Boolean,
    val printFiles: Boolean,
    val printUsage: Boolean,
    val printVersion: Boolean,
    val recursive: Boolean,
    val sortBy: SortBy,
    val sortCaseInsensitive: Boolean,
    val sortDescending: Boolean,
    val verbose: Boolean
) {
    override fun toString(): String {
        return "FindSettings(" +
                "archivesOnly=$archivesOnly, " +
                "debug=$debug, " +
                "inArchiveExtensions=${stringSetToString(inArchiveExtensions)}, " +
                "inArchiveFilePatterns=${patternSetToString(inArchiveFilePatterns)}, " +
                "inDirPatterns=${patternSetToString(inDirPatterns)}, " +
                "inExtensions=${stringSetToString(inExtensions)}, " +
                "inFilePatterns=${patternSetToString(inFilePatterns)}, " +
                "inFileTypes=${fileTypeSetToString(inFileTypes)}, " +
                "includeArchives=$includeArchives, " +
                "includeHidden=$includeHidden, " +
                "maxDepth=$maxDepth, " +
                "maxLastMod=${maxLastMod ?: 0}, " +
                "maxSize=$maxSize, " +
                "minDepth=$minDepth, " +
                "minLastMod=${minLastMod ?: 0}, " +
                "minSize=$minSize, " +
                "outArchiveExtensions=${stringSetToString(outArchiveExtensions)}, " +
                "outArchiveFilePatterns=${patternSetToString(outArchiveFilePatterns)}, " +
                "outDirPatterns=${patternSetToString(outDirPatterns)}, " +
                "outExtensions=${stringSetToString(outExtensions)}, " +
                "outFilePatterns=${patternSetToString(outFilePatterns)}, " +
                "outFileTypes=${fileTypeSetToString(outFileTypes)}, " +
                "paths=${stringSetToString(paths)}, " +
                "printDirs=$printDirs, " +
                "printFiles=$printFiles, " +
                "printUsage=$printUsage, " +
                "printVersion=$printVersion, " +
                "recursive=$recursive, " +
                "sortBy=$sortBy, " +
                "sortCaseInsensitive=$sortCaseInsensitive, " +
                "sortDescending=$sortDescending, " +
                "verbose=$verbose)"
    }
}

fun setToString(set: Set<String>, quotes: Boolean): String {
    val sb = StringBuilder("[")
    for ((i, s) in set.withIndex()) {
        if (i > 0) {
            sb.append(", ")
        }
        if (quotes) {
            sb.append("\"").append(s).append("\"")
        } else {
            sb.append(s)
        }
    }
    sb.append("]")
    return sb.toString()
}

fun stringSetToString(set: Set<String>): String {
    return setToString(set, true)
}

fun pathSetToString(set: Set<Path>): String {
    val stringSet = set.map { it.toString() }.toSet()
    return setToString(stringSet, true)
}

fun patternSetToString(set: Set<Regex>): String {
    val stringSet = set.map { it.toString() }.toSet()
    return setToString(stringSet, true)
}

fun fileTypeSetToString(set: Set<FileType>): String {
    val stringSet = set.map { it.value }.toSet()
    return setToString(stringSet, false)
}

fun getDefaultSettings(): FindSettings {
    return FindSettings(
        archivesOnly = false,
        debug = false,
        inArchiveExtensions = linkedSetOf(),
        inArchiveFilePatterns = linkedSetOf(),
        inDirPatterns = linkedSetOf(),
        inExtensions = linkedSetOf(),
        inFilePatterns = linkedSetOf(),
        inFileTypes = linkedSetOf(),
        includeArchives = false,
        includeHidden = false,
        maxDepth = -1,
        maxLastMod = null,
        maxSize = 0,
        minDepth = -1,
        minLastMod = null,
        minSize = 0,
        outArchiveExtensions = linkedSetOf(),
        outArchiveFilePatterns = linkedSetOf(),
        outDirPatterns = linkedSetOf(),
        outExtensions = linkedSetOf(),
        outFilePatterns = linkedSetOf(),
        outFileTypes = linkedSetOf(),
        paths = linkedSetOf(),
        printDirs = false,
        printFiles = false,
        printUsage = false,
        printVersion = false,
        recursive = true,
        sortBy = SortBy.FILEPATH,
        sortCaseInsensitive = false,
        sortDescending = false,
        verbose = false
    )
}

fun addExtensions(ext: String, extensions: Set<String>): Set<String> {
    val exts = ext.split(',').filter { it.isNotEmpty() }
    return extensions.plus(exts)
}

fun addFileTypes(ft: String, fileTypes: Set<FileType>): Set<FileType> {
    val fts = ft.split(',').filter { it.isNotEmpty() }.map { FileType.forName(it) }
    return fileTypes.plus(fts)
}

fun addPath(p: String, paths: Set<String>): Set<String> {
    return paths.plus(p)
}

fun setArchivesOnly(ss: FindSettings, archivesOnly: Boolean): FindSettings {
    return ss.copy(archivesOnly = archivesOnly, includeArchives = archivesOnly || ss.includeArchives)
}

fun setDebug(ss: FindSettings, debug: Boolean): FindSettings {
    return ss.copy(debug = debug, verbose = debug || ss.verbose)
}

fun needLastMod(ss: FindSettings): Boolean {
    return ss.sortBy == SortBy.LASTMOD
            || ss.maxLastMod != null || ss.minLastMod != null
}

fun needSize(ss: FindSettings): Boolean {
    return ss.sortBy == SortBy.FILESIZE
            || ss.maxSize > 0 || ss.minSize > 0
}

fun getLastModFromString(lastModString: String): LocalDateTime {
    var lastMod: LocalDateTime? = null
    try {
        lastMod = LocalDateTime.parse(lastModString)
    } catch (e: DateTimeParseException) {
        try {
            val maxLastModDate = LocalDate.parse(lastModString, DateTimeFormatter.ISO_LOCAL_DATE)
            lastMod = maxLastModDate.atTime(0, 0, 0)
        } catch (e2: DateTimeParseException) {
            println("Unable to parse lastModString")
        }
    }
    return lastMod!!
}
