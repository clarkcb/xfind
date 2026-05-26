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
import kotlin.io.path.isSymbolicLink
import kotlin.io.path.name

/**
 * @author cary on 7/23/16.
 */
class Finder(val settings: FindSettings) {
    private val fileTypes: FileTypes = FileTypes()

    init {
        validateSettings(settings)
    }

    private fun validateSettings(settings: FindSettings) {
        if (settings.paths.isEmpty()) {
            throw FindException(FindError.STARTPATH_NOT_DEFINED.message)
        }
        for (path in settings.paths) {
            var p = path
            if (!Files.exists(p)) {
                p = FileUtil.expandPath(p)
                if (!Files.exists(p)) {
                    throw FindException(FindError.STARTPATH_NOT_FOUND.message)
                }
            }
            if (!Files.isReadable(p)) {
                throw FindException(FindError.STARTPATH_NOT_READABLE.message)
            }
            if (Files.isSymbolicLink(p)) {
                if (!settings.followSymlinks) {
                    throw FindException(FindError.STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
                }
            } else if (Files.isDirectory(p)) {
                if (!isTraversableDirPath(p)) {
                    throw FindException(FindError.STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
                }
            } else if (Files.isRegularFile(p)) {
                if (filterToFileResult(p) == null) {
                    throw FindException(FindError.STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
                }
            } else {
                // TODO: start path is unknown/invalid type
                throw FindException(FindError.STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
            }
        }
        if (settings.maxDepth > -1 && settings.maxDepth < settings.minDepth) {
            throw FindException(FindError.INVALID_RANGE_FOR_MINDEPTH_AND_MAXDEPTH.message)
        }
        if (settings.maxLastMod != null && settings.minLastMod != null
            && settings.maxLastMod < settings.minLastMod) {
            throw FindException(FindError.INVALID_RANGE_FOR_MINLASTMOD_AND_MAXLASTMOD.message)
        }
        if (settings.maxSize > 0 && settings.maxSize < settings.minSize) {
            throw FindException(FindError.INVALID_RANGE_FOR_MINSIZE_AND_MAXSIZE.message)
        }
    }

    private fun matchesAnyPattern(s: String, patternSet: Set<Regex>): Boolean {
        return patternSet.any { p -> p.containsMatchIn(s) }
    }

    private fun anyMatchesAnyPattern(
        sList: List<String>,
        patternSet: Set<Regex>
    ): Boolean {
        return sList.any { s -> matchesAnyPattern(s, patternSet) }
    }

    private fun emptyOrMatchesAnyPattern(s: String, patternSet: Set<Regex>): Boolean {
        return patternSet.isEmpty() || matchesAnyPattern(s, patternSet)
    }

    private fun emptyOrNotMatchesAnyPattern(s: String, patternSet: Set<Regex>): Boolean {
        return patternSet.isEmpty() || !matchesAnyPattern(s, patternSet)
    }

    private fun emptyOrAnyMatchesAnyPattern(sList: List<String>, patternSet: Set<Regex>): Boolean {
        return patternSet.isEmpty() || anyMatchesAnyPattern(sList, patternSet)
    }

    private fun emptyOrNotAnyMatchesAnyPattern(sList: List<String>, patternSet: Set<Regex>): Boolean {
        return patternSet.isEmpty() || !anyMatchesAnyPattern(sList, patternSet)
    }

    private fun emptyOrMatchesAnyString(s: String, stringSet: Set<String>): Boolean {
        return stringSet.isEmpty() || stringSet.contains(s)
    }

    private fun emptyOrNotMatchesAnyString(s: String, stringSet: Set<String>): Boolean {
        return stringSet.isEmpty() || !stringSet.contains(s)
    }

    private fun emptyOrMatchesAnyFileType(fileType: FileType, fileTypeSet: Set<FileType>): Boolean {
        return fileTypeSet.isEmpty() || fileTypeSet.contains(fileType)
    }

    private fun emptyOrNotMatchesAnyFileType(fileType: FileType, fileTypeSet: Set<FileType>): Boolean {
        return fileTypeSet.isEmpty() || !fileTypeSet.contains(fileType)
    }

    fun isMatchingPathBySymlink(path: Path): Boolean {
        return settings.followSymlinks || !path.isSymbolicLink()
    }

    fun isMatchingDirPathByHidden(dirPath: Path): Boolean {
        return settings.includeHidden || !FileUtil.isHiddenPath(dirPath)
    }

    fun isMatchingDirPathByInPatterns(dirPath: Path): Boolean {
        val pathElems = FileUtil.splitPath(dirPath)
        return emptyOrAnyMatchesAnyPattern(pathElems, settings.inDirPatterns)
    }

    fun isMatchingDirPathByOutPatterns(dirPath: Path): Boolean {
        val pathElems = FileUtil.splitPath(dirPath)
        return emptyOrNotAnyMatchesAnyPattern(pathElems, settings.outDirPatterns)
    }

    fun isTraversableDirPath(dirPath: Path): Boolean {
        return isMatchingDirPathByHidden(dirPath)
                && isMatchingDirPathByOutPatterns(dirPath)
    }

    fun isMatchingDirPath(dirPath: Path): Boolean {
        return isMatchingDirPathByHidden(dirPath)
                && isMatchingDirPathByInPatterns(dirPath)
                && isMatchingDirPathByOutPatterns(dirPath)
    }

    fun isNullOrMatchingDirPath(dirPath: Path?): Boolean {
        // null or empty path is a match
        if (dirPath == null || dirPath.toString().isEmpty()) {
            return true
        }
        return isMatchingDirPath(dirPath)
    }

    fun isMatchingFileNameByHidden(fileName: String): Boolean {
        return settings.includeHidden || !FileUtil.isHiddenName(fileName)
    }

    fun isMatchingArchiveExtension(ext: String): Boolean {
        return emptyOrMatchesAnyString(ext, settings.inArchiveExtensions)
                && emptyOrNotMatchesAnyString(ext, settings.outArchiveExtensions)
    }

    fun isMatchingArchiveExtensionForFilePath(filePath: Path): Boolean {
        if (!settings.inArchiveExtensions.isEmpty() || !settings.outArchiveExtensions.isEmpty()) {
            return isMatchingArchiveExtension(filePath.extension)
        }
        return true
    }

    fun isMatchingArchiveFileName(fileName: String): Boolean {
        return emptyOrMatchesAnyPattern(fileName, settings.inArchiveFilePatterns)
                && emptyOrNotMatchesAnyPattern(fileName, settings.outArchiveFilePatterns)
    }

    fun isMatchingArchiveFileNameForFilePath(filePath: Path): Boolean {
        if (!settings.inArchiveFilePatterns.isEmpty() || !settings.outArchiveFilePatterns.isEmpty()) {
            val fileName = if (filePath.fileName == null) "" else filePath.fileName.toString()
            return isMatchingArchiveFileName(fileName)
        }
        return true
    }

    fun isMatchingArchiveFilePath(filePath: Path): Boolean {
        return isMatchingArchiveExtensionForFilePath(filePath)
                && isMatchingArchiveFileNameForFilePath(filePath)
    }

    fun isMatchingArchiveFileResult(fr: FileResult): Boolean {
        return isNullOrMatchingDirPath(fr.path.parent)
                && isMatchingArchiveFilePath(fr.path)
    }

    fun isMatchingExtension(ext: String): Boolean {
        return emptyOrMatchesAnyString(ext, settings.inExtensions)
                && emptyOrNotMatchesAnyString(ext, settings.outExtensions)
    }

    fun isMatchingExtensionForFilePath(filePath: Path): Boolean {
        if (!settings.inExtensions.isEmpty() || !settings.outExtensions.isEmpty()) {
            return isMatchingExtension(filePath.extension)
        }
        return true
    }

    fun isMatchingFileName(fileName: String): Boolean {
        return emptyOrMatchesAnyPattern(fileName, settings.inFilePatterns)
                && emptyOrNotMatchesAnyPattern(fileName, settings.outFilePatterns)
    }

    fun isMatchingFileNameForFilePath(filePath: Path): Boolean {
        if (!settings.inFilePatterns.isEmpty() || !settings.outFilePatterns.isEmpty()) {
            return isMatchingFileName(filePath.fileName.toString())
        }
        return true
    }

    fun isMatchingFilePath(filePath: Path): Boolean {
        // We assume that isNullOrMatchingDirPath(filePath.getParent()) has already been called
        return isMatchingExtensionForFilePath(filePath)
                && isMatchingFileNameForFilePath(filePath)
    }

    fun isMatchingFileType(fileType: FileType): Boolean {
        return emptyOrMatchesAnyFileType(fileType, settings.inFileTypes)
                && emptyOrNotMatchesAnyFileType(fileType, settings.outFileTypes)
    }

    fun isMatchingFileSize(fileSize: Long): Boolean {
        return ((settings.maxSize <= 0 || fileSize <= settings.maxSize)
                && (settings.minSize <= 0 || fileSize >= settings.minSize))
    }

    fun isMatchingLastMod(lastMod: Instant?): Boolean {
        if (settings.maxLastMod == null && settings.minLastMod == null) {
            return true
        }
        if (lastMod == null) {
            return false
        }
        if (settings.maxLastMod != null && lastMod > settings.maxLastMod.toInstant(ZoneOffset.UTC)) {
            return false
        }
        return ((settings.minLastMod == null)
                || lastMod >= settings.minLastMod.toInstant(ZoneOffset.UTC))
    }

    fun isMatchingLastMod(lastMod: FileTime?): Boolean {
        val lastModInstant = lastMod?.toInstant()
        return isMatchingLastMod(lastModInstant)
    }

    fun isMatchingFileResult(fr: FileResult): Boolean {
        return isNullOrMatchingDirPath(fr.path.parent)
                && isMatchingFilePath(fr.path)
                && isMatchingFileType(fr.fileType)
                && isMatchingFileSize(fr.fileSize)
                && isMatchingLastMod(fr.lastMod?.toInstant())
    }

    fun filterArchiveFilePathToFileResult(filePath: Path, fileType: FileType): FileResult? {
        if (!settings.includeArchives && !settings.archivesOnly) {
            return null
        }
        if (!isMatchingArchiveFilePath(filePath)) {
            return null
        }
        return FileResult(filePath, fileType, 0L, null)
    }

    fun filterRegularFilePathToFileResult(filePath: Path, fileType: FileType): FileResult? {
        if (settings.archivesOnly) {
            return null
        }

        if (!isMatchingFilePath(filePath) || !isMatchingFileType(fileType)) {
            return null
        }

        var fileSize = 0L
        var lastMod: FileTime? = null
        if (needLastMod(settings) || needSize(settings)) {
            try {
                val stat: BasicFileAttributes = Files.readAttributes(filePath, BasicFileAttributes::class.java)
                if (needSize(settings)) fileSize = stat.size()
                if (needLastMod(settings)) lastMod = stat.lastModifiedTime()
            } catch (e: Exception) {
                logError(e.message!!)
                return null
            }
        }

        if (!isMatchingFileSize(fileSize) || !isMatchingLastMod(lastMod)) {
            return null
        }

        return FileResult(filePath, fileType, fileSize, lastMod)
    }

    fun filterToFileResult(filePath: Path): FileResult? {
        if (!isNullOrMatchingDirPath(filePath.parent)
            || !isMatchingFileNameByHidden(filePath.fileName.toString())) {
            return null
        }

        val fileType = fileTypes.getFileType(filePath)
        if (fileType == FileType.ARCHIVE) {
            return filterArchiveFilePathToFileResult(filePath, fileType)
        }
        return filterRegularFilePathToFileResult(filePath, fileType)
    }

    private fun recFindPath(path: Path, minDepth: Int, maxDepth: Int, currentDepth: Int): List<FileResult> {
        var recurse = true
        if (currentDepth == maxDepth) {
            recurse = false
        } else if (maxDepth > -1 && currentDepth > maxDepth) {
            return emptyList()
        }
        val pathDirs: MutableList<Path> = mutableListOf()
        val pathResults: MutableList<FileResult> = mutableListOf()
        try {
            Files.newDirectoryStream(path).use { stream ->
                stream.forEach {
                    if (!Files.isSymbolicLink(it) || settings.followSymlinks) {
                        if (Files.isDirectory(it) && recurse && isTraversableDirPath(it)) {
                            pathDirs.add(it)
                        } else if (Files.isRegularFile(it) && (minDepth < 0 || currentDepth >= minDepth)) {
                            val fr = filterToFileResult(it)
                            if (fr != null) {
                                pathResults.add(fr)
                            }
                        }
                    }
                }
            }
        } catch (_: Exception) {
            return emptyList()
        }

        pathDirs.forEach {
            pathResults.addAll(recFindPath(it, minDepth, maxDepth, currentDepth + 1))
        }

        return pathResults
    }

    private fun findPath(path: Path): List<FileResult> {
        var p = path
        if (!Files.exists(path)) {
            p = FileUtil.expandPath(path)
            if (!Files.exists(p)) {
                throw FindException(FindError.STARTPATH_NOT_FOUND.message)
            }
        }
        if (Files.isSymbolicLink(p) && !settings.followSymlinks) {
            throw FindException(FindError.STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
        }
        if (Files.isDirectory(p)) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (settings.maxDepth == 0) {
                return emptyList()
            }
            if (isTraversableDirPath(p)) {
                val maxDepth = if (settings.recursive) settings.maxDepth else 1
                return recFindPath(p, settings.minDepth, maxDepth, 1)
            } else {
                throw FindException(FindError.STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
            }
        } else if (Files.isRegularFile(p)) {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (settings.minDepth > 0) {
                return emptyList()
            }
            val fr = filterToFileResult(p)
            if (fr != null) {
                return listOf(fr)
            } else {
                throw FindException(FindError.STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
            }
        } else {
            throw FindException(FindError.STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS.message)
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
        if (settings.paths.size > 1) {
            findAsync(fileResults)
        } else {
            fileResults.addAll(findPath(settings.paths.first()))
        }
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
