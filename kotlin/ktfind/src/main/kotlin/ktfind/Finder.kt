package ktfind

import java.io.File
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
    }

    private fun setTests() {
        if (settings.inExtensions.isNotEmpty()) {
            extTests.add { ext: String -> settings.inExtensions.contains(ext) }
        } else if (settings.outExtensions.isNotEmpty()) {
            extTests.add { ext: String -> !settings.outExtensions.contains(ext) }
        }
        if (settings.inFilePatterns.isNotEmpty()) {
            fileNameTests.add { fileName: String -> matchesAnyPattern(fileName, settings.inFilePatterns) }
        } else if (settings.outFilePatterns.isNotEmpty()) {
            fileNameTests.add { fileName: String -> !matchesAnyPattern(fileName, settings.outFilePatterns) }
        }
        if (settings.inFileTypes.isNotEmpty()) {
            fileTypeTests.add { fileType: FileType -> settings.inFileTypes.contains(fileType) }
        } else if (settings.outFileTypes.isNotEmpty()) {
            fileTypeTests.add { fileType: FileType -> !settings.outFileTypes.contains(fileType) }
        }
    }

    private fun anyMatchesAnyPattern(sList: List<String>,
                                     patternSet: Set<Regex>): Boolean {
        return sList.any { s -> matchesAnyPattern(s, patternSet) }
    }

    private fun matchesAnyPattern(s: String, patternSet: Set<Regex>): Boolean {
        return patternSet.any { p -> p.containsMatchIn(s) }
    }

    fun isMatchingDir(d: File): Boolean {
        val pathElems = d.path.split(File.separatorChar)
        if (settings.excludeHidden && pathElems.any { FileUtil.isHidden(it) }) {
            return false
        }
        return (settings.inDirPatterns.isEmpty()
                        || anyMatchesAnyPattern(pathElems, settings.inDirPatterns))
                &&
                (settings.outDirPatterns.isEmpty()
                        || !anyMatchesAnyPattern(pathElems, settings.outDirPatterns))
    }

//    fun isMatchingFile(fr: FileResult): Boolean {
//        return (settings.inExtensions.isEmpty()
//                        || settings.inExtensions.contains(fr.file.extension))
//                &&
//                (settings.outExtensions.isEmpty()
//                        || !settings.outExtensions.contains(fr.file.extension))
//                &&
//                (settings.inFilePatterns.isEmpty()
//                        || matchesAnyPattern(fr.file.name, settings.inFilePatterns))
//                &&
//                (settings.outFilePatterns.isEmpty()
//                        || !matchesAnyPattern(fr.file.name, settings.outFilePatterns))
//                &&
//                (settings.inFileTypes.isEmpty()
//                        || settings.inFileTypes.contains(fr.fileType))
//                &&
//                (settings.outFileTypes.isEmpty()
//                        || !settings.outFileTypes.contains(fr.fileType))
//    }

    fun isMatchingFile(fr: FileResult): Boolean {
        return (extTests.isEmpty()
                        || extTests.any { t -> t.invoke(fr.path.extension) })
                &&
                (fileNameTests.isEmpty()
                        || fileNameTests.any {t -> t.invoke(fr.path.fileName.name)})
                &&
                (fileTypeTests.isEmpty()
                        || fileTypeTests.any {t -> t.invoke(fr.fileType)})
    }

    fun isMatchingArchiveFile(fr: FileResult): Boolean {
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
        if (settings.excludeHidden && f.isHidden) {
            return null
        }
        val fr = FileResult(f.toPath(), fileTypes.getFileType(f))
        if (fr.fileType === FileType.ARCHIVE) {
            if ((settings.includeArchives || settings.archivesOnly) && isMatchingArchiveFile(fr)) {
                return fr;
            }
            return null;
        }
        if (!settings.archivesOnly && isMatchingFile(fr)) {
            return fr
        }
        return null
    }

    private fun getFileResults(startPath: File): List<FileResult> {
        return startPath.walk()
            .onEnter { isMatchingDir(it) }
            .filter { it.isFile }
            .mapNotNull { filterToFileResult(it) }
            .toList()
    }

    private fun sortFileResults(fileResults: List<FileResult>): List<FileResult> {
        val sortedFileResults: MutableList<FileResult> =
            when (settings.sortBy) {
                SortBy.FILENAME -> {
                    fileResults.stream().sorted { fr1, fr2 -> fr1.compareByName(fr2) }.toList()
                }
                SortBy.FILETYPE -> {
                    fileResults.stream().sorted { fr1, fr2 -> fr1.compareByType(fr2) }.toList()
                }
                else -> {
                    fileResults.stream().sorted { fr1, fr2 -> fr1.compareByPath(fr2) }.toList()
                }
            }.toMutableList()

        if (settings.sortDescending) {
            sortedFileResults.reverse()
        }

        return sortedFileResults.toList()
    }

    fun find(): List<FileResult> {
        val fileResults: MutableList<FileResult> = mutableListOf()
        for (p in settings.paths) {
            val pFile = File(p)
            if (pFile.isDirectory) {
                fileResults.addAll(getFileResults(pFile))
            } else if (pFile.isFile) {
                val fr = filterToFileResult(pFile)
                if (fr != null) {
                    fileResults.add(fr)
                } else {
                    throw FindException("Startpath does not match find settings")
                }
            } else {
                throw FindException("Path is invalid file type: $p")
            }
        }
        return sortFileResults(fileResults.toList())
    }
}
