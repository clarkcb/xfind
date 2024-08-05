package ktfind

import kotlinx.coroutines.*
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.attribute.FileTime
import java.time.Instant
import java.time.ZoneOffset
import kotlin.io.path.*
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
            if (needSize(settings)) fileSize = stat.size()
            if (needLastMod(settings)) lastMod = stat.lastModifiedTime()
        }
        val fr = FileResult(f.toPath(), fileTypes.getFileType(f.toPath()), fileSize, lastMod)
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

    private fun getPathFileResults(startPath: Path): List<FileResult> {
        val startPathSepCount = FileUtil.sepCount(startPath.toString())
        return startPath.toFile().walk()
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
                        fileResults.addAll(getPathFileResults(path))
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
                    throw FindException("Path is invalid file type: $path")
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
