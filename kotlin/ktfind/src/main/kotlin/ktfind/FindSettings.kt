package ktfind

/**
 * @author cary on 7/23/16.
 */
data class FindSettings(val archivesOnly: Boolean,
                          val colorize: Boolean,
                          val debug: Boolean,
                          val excludeHidden: Boolean,
                          val firstMatch: Boolean,
                          val inArchiveExtensions: Set<String>,
                          val inArchiveFilePatterns: Set<Regex>,
                          val inDirPatterns: Set<Regex>,
                          val inExtensions: Set<String>,
                          val inFilePatterns: Set<Regex>,
                          val inFileTypes: Set<FileType>,
                          val inLinesAfterPatterns: Set<Regex>,
                          val inLinesBeforePatterns: Set<Regex>,
                          val linesAfter: Int,
                          val linesAfterToPatterns: Set<Regex>,
                          val linesAfterUntilPatterns: Set<Regex>,
                          val linesBefore: Int,
                          val listDirs: Boolean,
                          val listFiles: Boolean,
                          val listLines: Boolean,
                          val maxLineLength: Int,
                          val multiLineFind: Boolean,
                          val outArchiveExtensions: Set<String>,
                          val outArchiveFilePatterns: Set<Regex>,
                          val outDirPatterns: Set<Regex>,
                          val outExtensions: Set<String>,
                          val outFilePatterns: Set<Regex>,
                          val outFileTypes: Set<FileType>,
                          val outLinesAfterPatterns: Set<Regex>,
                          val outLinesBeforePatterns: Set<Regex>,
                          val printResults: Boolean,
                          val printUsage: Boolean,
                          val printVersion: Boolean,
                          val recursive: Boolean,
                          val findArchives: Boolean,
                          val findPatterns: Set<Regex>,
                          val startPath: String?,
                          val textFileEncoding: String,
                          val uniqueLines: Boolean,
                          val verbose: Boolean)

fun getDefaultSettings() : FindSettings {
    return FindSettings(
            archivesOnly = false,
            colorize = true,
            debug = false,
            excludeHidden = true,
            firstMatch = false,
            inArchiveExtensions = setOf(),
            inArchiveFilePatterns = setOf(),
            inDirPatterns = setOf(),
            inExtensions = setOf(),
            inFilePatterns = setOf(),
            inFileTypes = setOf(),
            inLinesAfterPatterns = setOf(),
            inLinesBeforePatterns = setOf(),
            linesAfter = 0,
            linesAfterToPatterns = setOf(),
            linesAfterUntilPatterns = setOf(),
            linesBefore = 0,
            listDirs = false,
            listFiles = false,
            listLines = false,
            maxLineLength = 150,
            multiLineFind = false,
            outArchiveExtensions = setOf(),
            outArchiveFilePatterns = setOf(),
            outDirPatterns = setOf(),
            outExtensions = setOf(),
            outFilePatterns = setOf(),
            outFileTypes = setOf(),
            outLinesAfterPatterns = setOf(),
            outLinesBeforePatterns = setOf(),
            printResults = false,
            printUsage = false,
            printVersion = false,
            recursive = true,
            findArchives = false,
            findPatterns = setOf(),
            startPath = null,
            textFileEncoding = "UTF-8",
            uniqueLines = false,
            verbose = false)
}

fun addExtensions(ext: String, extensions: Set<String>): Set<String> {
    val exts = ext.split(',').filter { it.isNotEmpty() }
    return extensions.plus(exts)
}

fun addFileTypes(ft: String, filetypes: Set<FileType>): Set<FileType> {
    val fts = ft.split(',').filter { it.isNotEmpty() }.map { fromName(it) }
    return filetypes.plus(fts)
}

fun setArchivesOnly(ss: FindSettings, archivesOnly: Boolean): FindSettings {
    return ss.copy(archivesOnly = archivesOnly, findArchives = archivesOnly || ss.findArchives)
}

fun setDebug(ss: FindSettings, debug: Boolean): FindSettings {
    return ss.copy(debug = debug, verbose = debug || ss.verbose)
}
