package ktfind

fun printUsage(findOptions: FindOptions) {
    log("")
    findOptions.usage()
}

fun printErrorWithUsage(err: String, findOptions: FindOptions) {
    log("")
    logError(err + "\n")
    findOptions.usage()
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

fun find(settings: FindSettings) {
    val finder = Finder(settings)
    val fileResults: List<FileResult> = finder.find()
    val formatter = FileResultFormatter(settings)

    if (settings.printDirs) {
        printMatchingDirs(fileResults, formatter)
    }
    if (settings.printFiles) {
        printMatchingFiles(fileResults, formatter)
    }
}

fun main(args: Array<String>) {
    val findOptions = FindOptions()
    try {
        val settings = findOptions.settingsFromArgs(args)
        if (settings.debug) log("settings: $settings")
        if (settings.printUsage) printUsage(findOptions)
        else find(settings)
    } catch (e: FindException) {
        printErrorWithUsage(e.message ?: "Unknown error", findOptions)
    }
}
