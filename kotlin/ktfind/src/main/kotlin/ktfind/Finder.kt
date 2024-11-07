package ktfind

import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.attribute.FileTime
import java.time.Instant
import java.time.ZoneOffset
import kotlin.io.path.extension
import kotlin.io.path.name
import kotlin.streams.toList

/**
 * @author cary on 7/23/16.
 */
class Finder(val settings: FindSettings) {
    private val fileTypes: FileTypes
    private val extTests: MutableSet<(String) -> Boolean> = mutableSetOf()
    private val fileNameTests: MutableSet<(String) -> Boolean> = mutableSetOf()
    private val fileTypeTests: MutableSet<(FileType) -> Boolean> = mutableSetOf()

    init {
        validateSettings(settings)
        fileTypes = FileTypes()
        setTests()
    }

    private fun validateSettings(settings: FindSettings) {
        if (settings.paths.isEmpty()) {
            throw FindException("Startpath not defined")
        }
        for (p in settings.paths) {
            val path = Paths.get(p)
            if (!Files.exists(path)) {
                throw FindException("Startpath not found")
            }
            if (!Files.isReadable(path)) {
                throw FindException("Startpath not readable")
            }
        }
        if (settings.maxDepth > -1 && settings.maxDepth < settings.minDepth) {
            throw FindException("Invalid range for mindepth and maxdepth")
        }
        if (settings.maxLastMod != null && settings.minLastMod != null && settings.maxLastMod < settings.minLastMod) {
            throw FindException("Invalid range for minlastmod and maxlastmod")
        }
        if (settings.maxSize > 0 && settings.maxSize < settings.minSize) {
            throw FindException("Invalid range for minsize and maxsize")
        }
    }

    private fun setTests() {
        if (settings.inExtensions.isNotEmpty()) {
            extTests.add { ext: String -> settings.inExtensions.contains(ext) }
        } else if (settings.outExtensions.isNotEmpty()) {
            extTests.add { ext: String -> !settings.outExtensions.contains(ext) }
        }
        if (settings.inFilePatterns.isNotEmpty()) {
            fileNameTests.add { fileName: String -> matchesAnyPattern(fileName, settings.inFilePatterns) }
        }
        if (settings.outFilePatterns.isNotEmpty()) {
            fileNameTests.add { fileName: String -> !matchesAnyPattern(fileName, settings.outFilePatterns) }
        }
        if (settings.inFileTypes.isNotEmpty()) {
            fileTypeTests.add { fileType: FileType -> settings.inFileTypes.contains(fileType) }
        } else if (settings.outFileTypes.isNotEmpty()) {
            fileTypeTests.add { fileType: FileType -> !settings.outFileTypes.contains(fileType) }
        }
    }

    private fun anyMatchesAnyPattern(
        sList: List<String>,
        patternSet: Set<Regex>
    ): Boolean {
        return sList.any { s -> matchesAnyPattern(s, patternSet) }
    }

    private fun matchesAnyPattern(s: String, patternSet: Set<Regex>): Boolean {
        return patternSet.any { p -> p.containsMatchIn(s) }
    }

    fun isMatchingDir(path: Path?): Boolean {
        // null or empty path is a match
        if (path == null || path.toString().isEmpty()) {
            return true
        }
        if (!settings.includeHidden) {
            try {
                if (FileUtil.isHidden(path)) {
                    return false
                }
            } catch (e: Exception) {
                logError(e.message!!)
                return false
            }
        }
        val pathElems = FileUtil.splitPath(path)
        return (settings.inDirPatterns.isEmpty()
                || anyMatchesAnyPattern(pathElems, settings.inDirPatterns))
                &&
                (settings.outDirPatterns.isEmpty()
                        || !anyMatchesAnyPattern(pathElems, settings.outDirPatterns))
    }

    fun isMatchingArchiveExtension(ext: String): Boolean {
        return ((settings.inArchiveExtensions.isEmpty()
                || settings.inArchiveExtensions.contains(ext))
                &&
                (settings.outArchiveExtensions.isEmpty()
                        || !settings.outArchiveExtensions.contains(ext)))
    }

    fun isMatchingExtension(ext: String): Boolean {
        return ((settings.inExtensions.isEmpty()
                || settings.inExtensions.contains(ext))
                &&
                (settings.outExtensions.isEmpty()
                        || !settings.outExtensions.contains(ext)))
    }

    fun isMatchingArchiveFileName(fileName: String): Boolean {
        return ((settings.inArchiveFilePatterns.isEmpty()
                || matchesAnyPattern(fileName, settings.inArchiveFilePatterns))
                &&
                (settings.outArchiveFilePatterns.isEmpty()
                        || !matchesAnyPattern(fileName, settings.outArchiveFilePatterns)))
    }

    fun isMatchingFileName(fileName: String): Boolean {
        return ((settings.inFilePatterns.isEmpty()
                || matchesAnyPattern(fileName, settings.inFilePatterns))
                &&
                (settings.outFilePatterns.isEmpty()
                        || !matchesAnyPattern(fileName, settings.outFilePatterns)))
    }

    fun isMatchingFileType(fileType: FileType): Boolean {
        return ((settings.inFileTypes.isEmpty()
                || settings.inFileTypes.contains(fileType))
                &&
                (settings.outFileTypes.isEmpty()
                        || !settings.outFileTypes.contains(fileType)))
    }

    fun isMatchingFileSize(fileSize: Long): Boolean {
        return ((settings.maxSize <= 0 || fileSize <= settings.maxSize)
                && (settings.minSize <= 0 || fileSize >= settings.minSize))
    }

    fun isMatchingLastMod(lastMod: Instant?): Boolean {
        return (((settings.maxLastMod == null)
                || lastMod!! <= settings.maxLastMod!!.toInstant(ZoneOffset.UTC))
                && ((settings.minLastMod == null)
                || lastMod!! >= settings.minLastMod!!.toInstant(ZoneOffset.UTC)))
    }

    fun isMatchingFileResult(fr: FileResult): Boolean {
        return isMatchingExtension(fr.path.extension)
                && isMatchingFileName(fr.path.name)
                && isMatchingFileType(fr.fileType)
                && isMatchingFileSize(fr.fileSize)
                && isMatchingLastMod(fr.lastMod?.toInstant())
    }

    fun isMatchingArchiveFileResult(fr: FileResult): Boolean {
        return isMatchingArchiveExtension(fr.path.extension)
                && isMatchingArchiveFileName(fr.path.name)
    }

    fun filterToFileResult(p: Path): FileResult? {
        if (!settings.includeHidden) {
            try {
                if (Files.isHidden(p)) {
                    return null
                }
            } catch (e: Exception) {
                logError(e.message!!)
                return null
            }
        }

        val fileType = fileTypes.getFileType(p)
        if (fileType === FileType.ARCHIVE && !settings.includeArchives && !settings.archivesOnly) {
            return null
        }

        var fileSize = 0L
        var lastMod: FileTime? = null
        if (needLastMod(settings) || needSize(settings)) {
            try {
                val stat: BasicFileAttributes = Files.readAttributes(p, BasicFileAttributes::class.java)
                if (needSize(settings)) fileSize = stat.size()
                if (needLastMod(settings)) lastMod = stat.lastModifiedTime()
            } catch (e: Exception) {
                logError(e.message!!)
                return null
            }
        }

        val fr = FileResult(p, fileType, fileSize, lastMod)
        if (fr.fileType === FileType.ARCHIVE) {
            if (isMatchingArchiveFileResult(fr)) {
                return fr
            }
            return null
        }
        if (!settings.archivesOnly && isMatchingFileResult(fr)) {
            return fr
        }
        return null
    }

    private fun sortFileResults(fileResults: List<FileResult>): List<FileResult> {
        val sortedFileResults: MutableList<FileResult> =
            when (settings.sortBy) {
                SortBy.FILENAME -> {
                    fileResults.stream()
                        .sorted { fr1, fr2 -> fr1.compareByName(fr2, settings.sortCaseInsensitive) }.toList()
                }

                SortBy.FILESIZE -> {
                    fileResults.stream()
                        .sorted { fr1, fr2 -> fr1.compareBySize(fr2, settings.sortCaseInsensitive) }.toList()
                }

                SortBy.FILETYPE -> {
                    fileResults.stream()
                        .sorted { fr1, fr2 -> fr1.compareByType(fr2, settings.sortCaseInsensitive) }.toList()
                }

                SortBy.LASTMOD -> {
                    fileResults.stream()
                        .sorted { fr1, fr2 -> fr1.compareByLastMod(fr2, settings.sortCaseInsensitive) }.toList()
                }

                else -> {
                    fileResults.stream()
                        .sorted { fr1, fr2 -> fr1.compareByPath(fr2, settings.sortCaseInsensitive) }.toList()
                }
            }.toMutableList()

        if (settings.sortDescending) {
            sortedFileResults.reverse()
        }

        return sortedFileResults.toList()
    }

    private fun recFindPath(filePath: Path, minDepth: Int, maxDepth: Int, currentDepth: Int): List<FileResult> {
        var recurse = true
        if (currentDepth == maxDepth) {
            recurse = false
        } else if (maxDepth > -1 && currentDepth > maxDepth) {
            return emptyList()
        }
        val pathDirs: MutableList<Path> = mutableListOf()
        val pathResults: MutableList<FileResult> = mutableListOf()
        try {
            Files.newDirectoryStream(filePath).use { stream ->
                stream.forEach {
                    if (!Files.isSymbolicLink(it) || settings.followSymlinks) {
                        if (Files.isDirectory(it) && recurse && isMatchingDir(it)) {
                            pathDirs.add(it)
                        } else if (Files.isRegularFile(it) && (minDepth < 0 || currentDepth >= minDepth)) {
                            val fr = filterToFileResult(it)
                            if (fr != null) {
                                pathResults.add(fr)
                            }
                        }
                    }
                }
                pathDirs.forEach {
                    pathResults.addAll(recFindPath(it, minDepth, maxDepth, currentDepth + 1))
                }
            }
        } catch (e: Exception) {
            return emptyList()
        }

        return pathResults
    }

    private fun findPath(filePath: Path): List<FileResult> {
        if (Files.isDirectory(filePath)) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (settings.maxDepth == 0) {
                return emptyList()
            }
            if (isMatchingDir(filePath)) {
                val maxDepth = if (settings.recursive) settings.maxDepth else 1
                return recFindPath(filePath, settings.minDepth, maxDepth, 1)
            } else {
                throw FindException("Startpath does not match find settings")
            }
        } else if (Files.isRegularFile(filePath)) {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (settings.minDepth > 0) {
                return emptyList()
            }
            val fr = filterToFileResult(filePath)
            if (fr != null) {
                return listOf(fr)
            } else {
                throw FindException("Startpath does not match find settings")
            }
        } else {
            throw FindException("Startpath is not a findable file type")
        }
    }

    private fun findAsync(fileResults: MutableList<FileResult>): Unit = runBlocking {
        for (p in settings.paths) {
            launch {
                val path = Paths.get(p)
                fileResults.addAll(findPath(path))
            }
        }
    }

    fun find(): List<FileResult> {
        val fileResults: MutableList<FileResult> = mutableListOf()
        findAsync(fileResults)
        return sortFileResults(fileResults.toList())
    }
}
