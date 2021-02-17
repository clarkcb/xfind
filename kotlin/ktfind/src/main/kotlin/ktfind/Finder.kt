package ktfind

import java.io.File
import java.io.IOException
import java.nio.charset.Charset
import java.util.*

/**
 * @author cary on 7/23/16.
 */
class Finder(val settings: FindSettings) {
    private val fileTypes: FileTypes
    private var charset: Charset? = null

    init {
        validateSettings(settings)
        fileTypes = FileTypes()
    }

    private fun validateSettings(settings: FindSettings) {
        if (settings.startPath.isNullOrEmpty()) {
            throw FindException("Startpath not defined")
        }
        val startPathFile = File(settings.startPath)
        if (!startPathFile.exists()) {
            throw FindException("Startpath not found")
        }
        if (!startPathFile.canRead()) {
            throw FindException("Startpath not readable")
        }
        if (settings.findPatterns.isEmpty()) {
            throw FindException("No find patterns defined")
        }
        if (settings.linesAfter < 0) {
            throw FindException("Invalid linesafter")
        }
        if (settings.linesBefore < 0) {
            throw FindException("Invalid linesbefore")
        }
        if (settings.maxLineLength < 0) {
            throw FindException("Invalid maxlinelength")
        }
        try {
            charset = Charset.forName(settings.textFileEncoding)
        } catch (e: IllegalArgumentException) {
            throw FindException("Invalid or unsupported encoding: ${settings.textFileEncoding}")
        }
    }

    private fun anyMatchesAnyPattern(sList: List<String>,
                                     patternSet: Set<Regex>): Boolean {
        return sList.any { s -> matchesAnyPattern(s, patternSet) }
    }

    private fun matchesAnyPattern(s: String, patternSet: Set<Regex>): Boolean {
        return patternSet.any { p -> p.containsMatchIn(s) }
    }

    private fun findAnyMatches(s: String, startIndex: Int, patternSet: Set<Regex>): List<MatchResult> {
        return patternSet.mapNotNull { p -> p.find(s, startIndex) }
    }

    fun isFindDir(d: File): Boolean {
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

    fun isFindFile(sf: FindFile): Boolean {
        val ext = sf.file.extension
        return (settings.inExtensions.isEmpty()
                        || settings.inExtensions.contains(ext))
                &&
                (settings.outExtensions.isEmpty()
                        || !settings.outExtensions.contains(ext))
                &&
                (settings.inFilePatterns.isEmpty()
                        || matchesAnyPattern(sf.file.name, settings.inFilePatterns))
                &&
                (settings.outFilePatterns.isEmpty()
                        || !matchesAnyPattern(sf.file.name, settings.outFilePatterns))
                &&
                (settings.inFileTypes.isEmpty()
                        || settings.inFileTypes.contains(sf.fileType))
                &&
                (settings.outFileTypes.isEmpty()
                        || !settings.outFileTypes.contains(sf.fileType))
    }

    fun isArchiveFindFile(sf: FindFile): Boolean {
        val ext = sf.file.extension
        return (settings.inArchiveExtensions.isEmpty()
                        || settings.inArchiveExtensions.contains(ext))
                &&
                (settings.outArchiveExtensions.isEmpty()
                        || !settings.outArchiveExtensions.contains(ext))
                &&
                (settings.inArchiveFilePatterns.isEmpty()
                        || matchesAnyPattern(sf.file.name, settings.inArchiveFilePatterns))
                &&
                (settings.outArchiveFilePatterns.isEmpty()
                        || !matchesAnyPattern(sf.file.name, settings.outArchiveFilePatterns))
    }

    fun filterToFindFile(f: File): FindFile? {
        if (settings.excludeHidden && f.isHidden) {
            return null
        }
        val sf = FindFile(f, fileTypes.getFileType(f))
        if ((sf.fileType === FileType.ARCHIVE
                    && (settings.findArchives || settings.archivesOnly)
                    && isArchiveFindFile(sf))
            || (!settings.archivesOnly && isFindFile(sf))) {
            return sf
        }
        return null
    }

    private fun getFindFiles(startPath: File): List<FindFile> {
        return startPath.walk()
            .onEnter { isFindDir(it) }
            .filter { it.isFile }
            .mapNotNull { filterToFindFile(it) }
            .toList()
    }

    fun find(): List<FindResult> {
        val startPathFile = File(settings.startPath!!)
        return if (startPathFile.isDirectory) {
            findPath(startPathFile)
        } else if (startPathFile.isFile) {
            val sf = filterToFindFile(startPathFile)
            if (sf != null) {
                findFile(sf)
            } else {
                throw FindException("Startpath does not match find settings")
            }
        } else {
            throw FindException("startPath is invalid file type: " + settings.startPath)
        }
    }

    private fun findPath(filePath: File): List<FindResult> {
        val findFiles = getFindFiles(filePath)
        if (settings.verbose) {
            val findDirs: List<String> = findFiles.map { it.file.parent }.distinct().sorted().toList()
            log("\nDirectories to be found (${findDirs.size}):")
            for (d in findDirs) {
                log(d)
            }
            log("\n\nFiles to be found (${findFiles.size}):")
            for (sf in findFiles) {
                log(sf.toString())
            }
            log("")
        }
        return findFiles(findFiles)
    }

    private fun findFiles(sfs: List<FindFile>): List<FindResult> {
        return sfs.flatMap { findFile(it) }
    }

    private fun findFile(sf: FindFile): List<FindResult> {
        return when {
            setOf(FileType.TEXT, FileType.CODE, FileType.XML).contains(sf.fileType) -> {
                findTextFile(sf)
            }
            sf.fileType === FileType.BINARY -> {
                findBinaryFile(sf)
            }
            else -> {
                listOf()
            }
        }
    }

    private fun findTextFile(sf: FindFile): List<FindResult> {
        if (settings.debug) {
            log("Finding text file $sf")
        }
        return if (settings.multiLineFind) {
            findTextFileContents(sf)
        } else {
            findTextFileLines(sf)
        }
    }

    private fun findTextFileContents(sf: FindFile): List<FindResult> {
        val results: List<FindResult> =
                findMultilineString(sf.file.readText(charset!!))
        return results.map { r -> r.copy(file = sf) }
    }

    private fun getLinesFromMultiLineString(s: String, endLineIndices: List<Int>): List<String> {
        fun recGetLinesFromMultiLineString(indices: List<Int>, lines: List<String>): List<String> {
            return if (indices.size < 2) {
                lines
            } else {
                val nextLine = s.substring(indices.first() + 1, indices[1])
                recGetLinesFromMultiLineString(indices.drop(1), lines.plus(nextLine))
            }
        }
        return recGetLinesFromMultiLineString(endLineIndices, listOf())
    }

    private fun getLinesAfterFromMultiLineString(s: String,
                                                 afterNewlineIndices: List<Int>): List<String> {
        return if (afterNewlineIndices.isNotEmpty()) {
            val startIndex = afterNewlineIndices.first()
            if (settings.linesAfterToPatterns.isNotEmpty()) {
                val matches = findAnyMatches(s, startIndex,
                        settings.linesAfterToPatterns)
                if (matches.isNotEmpty()) {
                    val firstMatch = matches.minBy { it.range.first }
                    val count = afterNewlineIndices.count { it <= firstMatch!!.range.first }
                    getLinesFromMultiLineString(s, afterNewlineIndices.take(count + 1))
                } else listOf()
            } else if (settings.linesAfterUntilPatterns.isNotEmpty()) {
                val matches = findAnyMatches(s, startIndex,
                        settings.linesAfterUntilPatterns)
                if (matches.isNotEmpty()) {
                    val firstMatch = matches.minBy { it.range.first }
                    val count = afterNewlineIndices.count { it <= firstMatch!!.range.first }
                    getLinesFromMultiLineString(s, afterNewlineIndices.take(count))
                } else listOf()
            } else if (settings.linesAfter > 0) {
                val laIndices = afterNewlineIndices.take(settings.linesAfter + 1)
                getLinesFromMultiLineString(s, laIndices)
            } else listOf()
        } else listOf()
    }

    fun findMultilineString(s: String): List<FindResult> {
        val results: MutableList<FindResult> = mutableListOf()
        val newlineIndices: List<Int> = s.indices.zip(s.asIterable()).
                filter { it.second == '\n' }.map { it.first }

        for (p in settings.findPatterns) {
            val matches: Sequence<MatchResult> =
                    if (settings.firstMatch) {
                        val m = p.find(s)
                        if (m == null) {
                            sequenceOf()
                        } else {
                            sequenceOf(m)
                        }
                    }
                    else p.findAll(s)

            for (m in matches) {
                val beforeNewlineIndices = newlineIndices.takeWhile { it <= m.range.first }
                // val beforeLineCount = newlineIndices.count { it <= m.range.first }
                val linesBefore =
                        if (settings.linesBefore > 0) {
                            val lbIndices = beforeNewlineIndices.reversed().
                                    take(settings.linesBefore + 1).reversed()
                            getLinesFromMultiLineString(s, lbIndices)
                        } else listOf()
                val afterNewlineIndices = newlineIndices.dropWhile { it <= m.range.first }
                val linesAfter = getLinesAfterFromMultiLineString(s, afterNewlineIndices)
                if (!linesBeforeMatch(linesBefore) || !linesAfterMatch(linesAfter)) {
                    continue
                }
                val lineNum = beforeNewlineIndices.size + 1
                val startLineIndex =
                        if (lineNum == 1) 0
                        else beforeNewlineIndices.last() + 1
                val endLineIndex =
                        if (afterNewlineIndices.isNotEmpty()) afterNewlineIndices.first()
                        else s.length - 1
                val line = s.substring(startLineIndex, endLineIndex)
                val startMatchIndex = m.range.first - startLineIndex + 1
                val endMatchIndex = m.range.last - startLineIndex + 2
                results.add(FindResult(
                        p,
                        null,
                        lineNum,
                        startMatchIndex,
                        endMatchIndex,
                        line,
                        linesBefore,
                        linesAfter))
            }
        }
        return results.toList()
    }

    fun findTextFileLines(sf: FindFile): List<FindResult> {
        val results: List<FindResult> = sf.file.reader(charset!!).
                useLines { ss -> findLineIterator(ss.iterator()) }
        return results.map { r -> r.copy(file = sf) }
    }

    private fun linesMatch(lines: List<String>, inPatterns: Set<Regex>,
                   outPatterns: Set<Regex>): Boolean {
        return (inPatterns.isEmpty() || anyMatchesAnyPattern(lines, inPatterns))
                &&
                (outPatterns.isEmpty() || !anyMatchesAnyPattern(lines, outPatterns))

    }

    private fun linesBeforeMatch(linesBefore: List<String>): Boolean {
        return linesBefore.isEmpty()
                ||
                linesMatch(linesBefore, settings.inLinesBeforePatterns,
                        settings.outLinesBeforePatterns)
    }

    private fun linesAfterMatch(linesAfter: List<String>): Boolean {
        return linesAfter.isEmpty()
                ||
                linesMatch(linesAfter, settings.inLinesAfterPatterns,
                        settings.outLinesAfterPatterns)
    }

    private fun matchLinesAfterToOrUntil(linesAfter: MutableList<String>,
                                         lines: Iterator<String>): Boolean {
        if (settings.linesAfterToPatterns.isEmpty()
                && settings.linesAfterUntilPatterns.isEmpty())
            return true

        for (i: Int in linesAfter.indices) {
            if (matchesAnyPattern(linesAfter[i], settings.linesAfterToPatterns)) {
                while (i + 1 < linesAfter.size) linesAfter.removeAt(i + 1)
                return true
            } else if (matchesAnyPattern(linesAfter[i], settings.linesAfterUntilPatterns)) {
                while (i < linesAfter.size) linesAfter.removeAt(i)
                return true
            }
        }
        var foundMatch = false
        while (!foundMatch && lines.hasNext()) {
            val nextLine = lines.next()
            when {
                matchesAnyPattern(nextLine, settings.linesAfterToPatterns) -> {
                    linesAfter.add(nextLine)
                    foundMatch = true
                }
                matchesAnyPattern(nextLine, settings.linesAfterUntilPatterns) -> {
                    foundMatch = true
                }
                else -> {
                    linesAfter.add(nextLine)
                }
            }
        }
        return foundMatch
    }

    fun findLineIterator(lines: Iterator<String>): List<FindResult> {
        val results: MutableList<FindResult> = mutableListOf()
        val linesBefore: MutableList<String> = mutableListOf()
        val linesAfter: MutableList<String> = mutableListOf()
        var lineNum = 0
        val matchedPatterns : MutableSet<Regex> = mutableSetOf()
        while (true) {
            lineNum += 1
            val line =
                    if (linesAfter.isNotEmpty()) {
                        linesAfter.removeAt(0)
                    } else if (lines.hasNext()) {
                        lines.next()
                    } else {
                        break
                    }
            if (settings.linesAfter > 0) {
                while (linesAfter.size < settings.linesAfter && lines.hasNext()) {
                    linesAfter.add(lines.next())
                }
            }

            val findPatterns =
                    if (settings.firstMatch)
                        settings.findPatterns.filterNot { matchedPatterns.contains(it) }
                    else settings.findPatterns

            if (findPatterns.isEmpty()) {
                break
            }

            for (p in findPatterns) {
                val matches: Sequence<MatchResult> =
                        if (settings.firstMatch) {
                            val m = p.find(line)
                            if (m == null) {
                                sequenceOf()
                            } else {
                                sequenceOf(m)
                            }
                        }
                        else p.findAll(line)
                if (matches.count() == 0
                        || !linesBeforeMatch(linesBefore)
                        || !linesAfterMatch(linesAfter)
                        || !matchLinesAfterToOrUntil(linesAfter, lines.iterator())) {
                    continue
                }

                matchedPatterns.add(p)

                for (m in matches) {
                    results.add(FindResult(
                            p,
                            null,
                            lineNum,
                            m.range.first + 1,
                            m.range.last + 2,
                            line,
                            linesBefore.toList(),
                            linesAfter.toList()))
                }
            }
            if (settings.linesBefore > 0) {
                if (linesBefore.size == settings.linesBefore) {
                    linesBefore.removeAt(0)
                }
                if (linesBefore.size < settings.linesBefore) {
                    linesBefore.add(line)
                }
            }
            if (settings.firstMatch && matchedPatterns.size == settings.findPatterns.size) {
                break
            }
        }
        return results.toList()
    }

    private fun findBinaryFile(sf: FindFile): List<FindResult> {
        if (settings.verbose) {
            log("Finding binary file $sf")
        }
        val results: MutableList<FindResult> = mutableListOf()

        try {
            val content: String = sf.file.readText(Charsets.ISO_8859_1)
            for (p in settings.findPatterns) {
                val matches: Sequence<MatchResult> =
                        if (settings.firstMatch) {
                            val m = p.find(content)
                            if (m == null) {
                                sequenceOf()
                            } else {
                                sequenceOf(m)
                            }
                        }
                        else p.findAll(content)
                matches.forEach { m ->
                    results.add(FindResult(
                            p,
                            sf,
                            0,
                            m.range.first + 1,
                            m.range.last + 1,
                            ""))
                }
            }
        } catch (e: IOException) {
            log(e.toString())
        } catch (e: NoSuchElementException) {
            log(e.toString())
        } catch (e: IllegalStateException) {
            log(e.toString())
        }
        return results.toList()
    }
}
