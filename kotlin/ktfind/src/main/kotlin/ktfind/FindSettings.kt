package ktfind

/**
 * @author cary on 7/23/16.
 */
data class FindSettings(val archivesOnly: Boolean,
                          val colorize: Boolean,
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
                          val verbose: Boolean)

fun getDefaultSettings() : FindSettings {
    return FindSettings(
            archivesOnly = false,
            colorize = true,
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
    return ss.copy(archivesOnly = archivesOnly, includeArchives = archivesOnly || ss.includeArchives)
}

fun setDebug(ss: FindSettings, debug: Boolean): FindSettings {
    return ss.copy(debug = debug, verbose = debug || ss.verbose)
}
