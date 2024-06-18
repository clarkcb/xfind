package ktfind

import kotlinx.coroutines.*
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.attribute.FileTime
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
            val pFile = File(p)
            if (!pFile.exists()) {
                throw FindException("Startpath not found")
            }
            if (!pFile.canRead()) {
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

    fun isMatchingDir(d: File): Boolean {
        val pathElems = d.path.split(File.separatorChar)
        if (!settings.includeHidden && pathElems.any { FileUtil.isHidden(it) }) {
            return false
        }
        return (settings.inDirPatterns.isEmpty()
                || anyMatchesAnyPattern(pathElems, settings.inDirPatterns))
                &&
                (settings.outDirPatterns.isEmpty()
                        || !anyMatchesAnyPattern(pathElems, settings.outDirPatterns))
    }

    fun hasMatchingExtension(fr: FileResult): Boolean {
        return ((settings.inExtensions.isEmpty()
                || settings.inExtensions.contains(fr.path.extension))
                &&
                (settings.outExtensions.isEmpty()
                        || !settings.outExtensions.contains(fr.path.extension)))
    }

    fun hasMatchingFileName(fr: FileResult): Boolean {
        return ((settings.inFilePatterns.isEmpty()
                || matchesAnyPattern(fr.path.fileName.toString(), settings.inFilePatterns))
                &&
                (settings.outFilePatterns.isEmpty()
                        || !matchesAnyPattern(fr.path.fileName.toString(), settings.outFilePatterns)))
    }

    fun hasMatchingFileType(fr: FileResult): Boolean {
        return ((settings.inFileTypes.isEmpty()
                || settings.inFileTypes.contains(fr.fileType))
                &&
                (settings.outFileTypes.isEmpty()
                        || !settings.outFileTypes.contains(fr.fileType)))
    }

    fun hasMatchingFileSize(fr: FileResult): Boolean {
        return ((settings.maxSize <= 0 || fr.fileSize <= settings.maxSize)
                && (settings.minSize <= 0 || fr.fileSize >= settings.minSize))
    }

    fun hasMatchingLastMod(fr: FileResult): Boolean {
        return ((settings.maxLastMod == null || fr.lastMod == null
                || fr.lastMod.toInstant() <= settings.maxLastMod.toInstant(ZoneOffset.UTC))
                && (settings.minLastMod == null || fr.lastMod == null
                || fr.lastMod.toInstant() >= settings.minLastMod.toInstant(ZoneOffset.UTC)))
    }

    fun isMatchingFileResult(fr: FileResult): Boolean {
        return hasMatchingExtension(fr)
                && hasMatchingFileName(fr)
                && hasMatchingFileType(fr)
                && hasMatchingFileSize(fr)
                && hasMatchingLastMod(fr)
    }

    fun isMatchingArchiveFileResult(fr: FileResult): Boolean {
        return (settings.inArchiveExtensions.isEmpty()
                || settings.inArchiveExtensions.contains(fr.path.extension))
                &&
                (settings.outArchiveExtensions.isEmpty()
                        || !settings.outArchiveExtensions.contains(fr.path.extension))
                &&
                (settings.inArchiveFilePatterns.isEmpty()
                        || matchesAnyPattern(fr.path.name, settings.inArchiveFilePatterns))
                &&
                (settings.outArchiveFilePatterns.isEmpty()
                        || !matchesAnyPattern(fr.path.name, settings.outArchiveFilePatterns))
    }

    fun filterToFileResult(f: File): FileResult? {
        if (!settings.includeHidden && f.isHidden) {
            return null
        }
        var fileSize = 0L
        var lastMod: FileTime? = null
        if (needLastMod(settings) || needSize(settings)) {
            if (!f.canRead()) {
                return null
            }
            val stat: BasicFileAttributes = Files.readAttributes(f.toPath(), BasicFileAttributes::class.java)
            fileSize = stat.size()
            lastMod = stat.lastModifiedTime()
        }
        val fr = FileResult(f.toPath(), fileTypes.getFileType(f), fileSize, lastMod)
        if (fr.fileType === FileType.ARCHIVE) {
            if ((settings.includeArchives || settings.archivesOnly) && isMatchingArchiveFileResult(fr)) {
                return fr
            }
            return null
        }
        if (!settings.archivesOnly && isMatchingFileResult(fr)) {
            return fr
        }
        return null
    }

    // NOTE: we only look at minDepth here since maxDepth can be specified in FileTreeWalk
    private fun filterFile(f: File, startPathSepCount: Long): FileResult? {
        val fileSepCount = FileUtil.sepCount(f.toString())
        val depth = (fileSepCount - startPathSepCount).toInt()
        if (depth < settings.minDepth) return null
        return filterToFileResult(f)
    }

    private fun getFileResults(startPath: File): List<FileResult> {
        val startPathSepCount = FileUtil.sepCount(startPath.toString())
        return startPath.walk()
            .maxDepth(if (settings.maxDepth > 0) settings.maxDepth else Int.MAX_VALUE)
            .onEnter { isMatchingDir(it) }
            .filter { it.isFile }
            .mapNotNull { filterFile(it, startPathSepCount) }
            .toList()
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

    private fun findAsync(fileResults: MutableList<FileResult>): Unit = runBlocking {
        for (p in settings.paths) {
            launch {
                val path = Paths.get(p)
                if (Files.isDirectory(path)) {
                    // if maxDepth is zero, we can skip since a directory cannot be a result
                    if (settings.maxDepth != 0) {
                        fileResults.addAll(getFileResults(path.toFile()))
                    }
                } else if (Files.isReadable(path)) {
                    // if minDepth > zero, we can skip since the file is at depth zero
                    if (settings.minDepth <= 0) {
                        val fr = filterToFileResult(path.toFile())
                        if (fr != null) {
                            fileResults.add(fr)
                        } else {
                            throw FindException("Startpath does not match find settings")
                        }
                    }
                } else {
                    throw FindException("Path is invalid file type: $p")
                }
            }
        }
    }

    fun find(): List<FileResult> {
        val fileResults: MutableList<FileResult> = mutableListOf()
        findAsync(fileResults)
        return sortFileResults(fileResults.toList())
    }
}
