package ktfind

import java.io.File

/**
 * @author cary on 7/23/16.
 */
class Finder(val settings: FindSettings) {
    private val fileTypes: FileTypes

    init {
        validateSettings(settings)
        fileTypes = FileTypes()
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

    fun isMatchingFile(fr: FileResult): Boolean {
        val ext = fr.file.extension
        return (settings.inExtensions.isEmpty()
                        || settings.inExtensions.contains(ext))
                &&
                (settings.outExtensions.isEmpty()
                        || !settings.outExtensions.contains(ext))
                &&
                (settings.inFilePatterns.isEmpty()
                        || matchesAnyPattern(fr.file.name, settings.inFilePatterns))
                &&
                (settings.outFilePatterns.isEmpty()
                        || !matchesAnyPattern(fr.file.name, settings.outFilePatterns))
                &&
                (settings.inFileTypes.isEmpty()
                        || settings.inFileTypes.contains(fr.fileType))
                &&
                (settings.outFileTypes.isEmpty()
                        || !settings.outFileTypes.contains(fr.fileType))
    }

    fun isMatchingArchiveFile(fr: FileResult): Boolean {
        val ext = fr.file.extension
        return (settings.inArchiveExtensions.isEmpty()
                        || settings.inArchiveExtensions.contains(ext))
                &&
                (settings.outArchiveExtensions.isEmpty()
                        || !settings.outArchiveExtensions.contains(ext))
                &&
                (settings.inArchiveFilePatterns.isEmpty()
                        || matchesAnyPattern(fr.file.name, settings.inArchiveFilePatterns))
                &&
                (settings.outArchiveFilePatterns.isEmpty()
                        || !matchesAnyPattern(fr.file.name, settings.outArchiveFilePatterns))
    }

    fun filterToFileResult(f: File): FileResult? {
        if (settings.excludeHidden && f.isHidden) {
            return null
        }
        val fr = FileResult(f, fileTypes.getFileType(f))
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
        return fileResults.toList()
    }
}
