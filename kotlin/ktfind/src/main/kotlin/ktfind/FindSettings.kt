package ktfind

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException

/**
 * @author cary on 7/23/16.
 */
enum class SortBy {
    FILEPATH,
    FILENAME,
    FILESIZE,
    FILETYPE,
    LASTMOD
}

private const val NAME = "name"
private const val PATH = "path"
private const val SIZE = "size"
private const val TYPE = "type"
private const val LASTMOD = "lastmod"

fun sortByFromName(sortByName: String) : SortBy {
    return when (sortByName.trim().lowercase()) {
        NAME -> {
            SortBy.FILENAME
        }
        SIZE -> {
            SortBy.FILESIZE
        }
        TYPE -> {
            SortBy.FILETYPE
        }
        LASTMOD -> {
            SortBy.LASTMOD
        }
        else -> {
            SortBy.FILEPATH
        }
    }
}

data class FindSettings(val archivesOnly: Boolean,
                        val debug: Boolean,
                        val excludeHidden: Boolean,
                        val inArchiveExtensions: Set<String>,
                        val inArchiveFilePatterns: Set<Regex>,
                        val inDirPatterns: Set<Regex>,
                        val inExtensions: Set<String>,
                        val inFilePatterns: Set<Regex>,
                        val inFileTypes: Set<FileType>,
                        val includeArchives: Boolean,
                        val listDirs: Boolean,
                        val listFiles: Boolean,
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
                        val printUsage: Boolean,
                        val printVersion: Boolean,
                        val recursive: Boolean,
                        val sortBy: SortBy,
                        val sortCaseInsensitive: Boolean,
                        val sortDescending: Boolean,
                        val verbose: Boolean)

fun getDefaultSettings() : FindSettings {
    return FindSettings(
        archivesOnly = false,
        debug = false,
        excludeHidden = true,
        inArchiveExtensions = setOf(),
        inArchiveFilePatterns = setOf(),
        inDirPatterns = setOf(),
        inExtensions = setOf(),
        inFilePatterns = setOf(),
        inFileTypes = setOf(),
        includeArchives = false,
        listDirs = false,
        listFiles = false,
        maxDepth = -1,
        maxLastMod = null,
        maxSize = 0,
        minDepth = -1,
        minLastMod = null,
        minSize = 0,
        outArchiveExtensions = setOf(),
        outArchiveFilePatterns = setOf(),
        outDirPatterns = setOf(),
        outExtensions = setOf(),
        outFilePatterns = setOf(),
        outFileTypes = setOf(),
        paths = setOf(),
        printUsage = false,
        printVersion = false,
        recursive = true,
        sortBy = SortBy.FILEPATH,
        sortCaseInsensitive = false,
        sortDescending = false,
        verbose = false)
}

fun addExtensions(ext: String, extensions: Set<String>): Set<String> {
    val exts = ext.split(',').filter { it.isNotEmpty() }
    return extensions.plus(exts)
}

fun addFileTypes(ft: String, fileTypes: Set<FileType>): Set<FileType> {
    val fts = ft.split(',').filter { it.isNotEmpty() }.map { fileTypeFromName(it) }
    return fileTypes.plus(fts)
}

fun setArchivesOnly(ss: FindSettings, archivesOnly: Boolean): FindSettings {
    return ss.copy(archivesOnly = archivesOnly, includeArchives = archivesOnly || ss.includeArchives)
}

fun setDebug(ss: FindSettings, debug: Boolean): FindSettings {
    return ss.copy(debug = debug, verbose = debug || ss.verbose)
}

fun needStat(ss: FindSettings): Boolean {
    return ss.sortBy == SortBy.FILESIZE || ss.sortBy == SortBy.LASTMOD
            || ss.maxLastMod != null || ss.minLastMod != null
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
