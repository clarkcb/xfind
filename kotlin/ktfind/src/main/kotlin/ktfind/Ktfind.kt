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

fun printMatchingDirs(fileResults: List<FileResult>) {
    val dirs = fileResults.mapNotNull { f -> f.path.parent }.distinct().sorted()
    if (dirs.isEmpty()) {
        log("\nMatching directories: 0")
    } else {
        log("\nMatching directories (${dirs.size}):")
        for (d in dirs) {
            log(d.toString())
        }
    }
}

fun printMatchingFiles(fileResults: List<FileResult>) {
    val files = fileResults.map { fr -> fr.toString() }
    if (files.isEmpty()) {
        log("\nMatching files: 0")
    } else {
        log("\nMatching files (${files.size}):")
        for (f in files) {
            log(f)
        }
    }
}

fun find(settings: FindSettings) {
    val finder = Finder(settings)
    val fileResults: List<FileResult> = finder.find()

    if (settings.printDirs) {
        printMatchingDirs(fileResults)
    }
    if (settings.printFiles) {
        printMatchingFiles(fileResults)
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
