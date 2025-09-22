package ktfind

import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.attribute.FileTime
import java.time.Instant
import java.time.ZoneOffset
import kotlin.io.path.extension
import kotlin.io.path.name

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
            throw FindException(FindError.STARTPATH_NOT_DEFINED.message)
        }
        for (path in settings.paths) {
            var p = path
            if (!Files.exists(p)) {
                p = FileUtil.expandPath(p)
            }
            if (Files.exists(p)) {
                if (!Files.isReadable(p)) {
                    throw FindException(FindError.STARTPATH_NOT_READABLE.message)
                }
            } else {
                throw FindException(FindError.STARTPATH_NOT_FOUND.message)
            }
        }
        if (settings.maxDepth > -1 && settings.maxDepth < settings.minDepth) {
            throw FindException(FindError.INVALID_RANGE_FOR_MINDEPTH_AND_MAXDEPTH.message)
        }
        if (settings.maxLastMod != null && settings.minLastMod != null && settings.maxLastMod < settings.minLastMod) {
            throw FindException(FindError.INVALID_RANGE_FOR_MINLASTMOD_AND_MAXLASTMOD.message)
        }
        if (settings.maxSize > 0 && settings.maxSize < settings.minSize) {
            throw FindException(FindError.INVALID_RANGE_FOR_MINSIZE_AND_MAXSIZE.message)
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

    fun filterDirByHidden(path: Path?): Boolean {
        // null or empty path is a match
        if (path == null || path.toString().isEmpty()) {
            return true
        }
        if (!settings.includeHidden) {
            return !FileUtil.isHiddenPath(path)
        }
        return true
    }

    fun filterDirByInPatterns(path: Path?): Boolean {
        // null or empty path is a match
        if (path == null || path.toString().isEmpty()) {
            return true
        }
        val pathElems = FileUtil.splitPath(path)
        return (settings.inDirPatterns.isEmpty()
                || anyMatchesAnyPattern(pathElems, settings.inDirPatterns))
    }

    fun filterDirByOutPatterns(path: Path?): Boolean {
        // null or empty path is a match
        if (path == null || path.toString().isEmpty()) {
            return true
        }
        val pathElems = FileUtil.splitPath(path)
        return (settings.outDirPatterns.isEmpty()
                || !anyMatchesAnyPattern(pathElems, settings.outDirPatterns))
    }

    fun isMatchingDir(path: Path?): Boolean {
        // null or empty path is a match
        if (path == null || path.toString().isEmpty()) {
            return true
        }
        return filterDirByHidden(path) && filterDirByInPatterns(path) && filterDirByOutPatterns(path)
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
                || lastMod!! <= settings.maxLastMod.toInstant(ZoneOffset.UTC))
                && ((settings.minLastMod == null)
                || lastMod!! >= settings.minLastMod.toInstant(ZoneOffset.UTC)))
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
        if (!isMatchingDir(p.parent)) {
            return null
        }

        if (!settings.includeHidden && FileUtil.isHiddenName(p.name)) {
            return null
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
                        if (Files.isDirectory(it) && recurse && filterDirByHidden(it) && filterDirByOutPatterns(it)) {
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
        } catch (_: Exception) {
            return emptyList()
        }

        return pathResults
    }

    private fun findPath(filePath: Path): List<FileResult> {
        val fp = if (Files.exists(filePath)) filePath else FileUtil.expandPath(filePath)
        if (Files.isDirectory(fp)) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (settings.maxDepth == 0) {
                return emptyList()
            }
            if (filterDirByHidden(fp) && filterDirByOutPatterns(fp)) {
                val maxDepth = if (settings.recursive) settings.maxDepth else 1
                return recFindPath(fp, settings.minDepth, maxDepth, 1)
            } else {
                throw FindException(FindError.STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
            }
        } else {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (settings.minDepth > 0) {
                return emptyList()
            }
            val fr = filterToFileResult(fp)
            if (fr != null) {
                return listOf(fr)
            } else {
                throw FindException(FindError.STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
            }
        }
    }

    private fun findAsync(fileResults: MutableList<FileResult>): Unit = runBlocking {
        for (path in settings.paths) {
            launch {
                fileResults.addAll(findPath(path))
            }
        }
    }

    fun find(): List<FileResult> {
        val fileResults: MutableList<FileResult> = mutableListOf()
        findAsync(fileResults)
        if (fileResults.size > 1) {
            val fileResultSorter = FileResultSorter(settings)
            return fileResultSorter.sort(fileResults.toList())
        }
        return fileResults.toList()
    }

    fun printMatchingDirs(fileResults: List<FileResult>, formatter: FileResultFormatter) {
        val dirs = fileResults.mapNotNull { f -> f.path.parent }.distinct().sorted()
        if (dirs.isEmpty()) {
            log("\nMatching directories: 0")
        } else {
            log("\nMatching directories (${dirs.size}):")
            for (d in dirs) {
                log(formatter.formatDirPath(d))
            }
        }
    }

    fun printMatchingFiles(fileResults: List<FileResult>, formatter: FileResultFormatter) {
        if (fileResults.isEmpty()) {
            log("\nMatching files: 0")
        } else {
            log("\nMatching files (${fileResults.size}):")
            for (fr in fileResults) {
                log(formatter.formatFileResult(fr))
            }
        }
    }
}
