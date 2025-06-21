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

fun find(settings: FindSettings) {
    val finder = Finder(settings)
    val fileResults: List<FileResult> = finder.find()
    val formatter = FileResultFormatter(settings)

    if (settings.printDirs) {
        finder.printMatchingDirs(fileResults, formatter)
    }
    if (settings.printFiles) {
        finder.printMatchingFiles(fileResults, formatter)
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
