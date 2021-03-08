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

private fun signum(num: Int): Int {
    if (num > 0) {
        return 1
    }
    if (num < 0) {
        return -1
    }
    return 0
}

fun printMatchingDirs(findFiles: List<FindFile>) {
    val dirs = findFiles.mapNotNull { f -> f.file.parent }.distinct().sorted()
    if (dirs.isEmpty()) {
        log("\nMatching directories: 0")
    } else {
        log("\nMatching directories (${dirs.size}):")
        for (d in dirs) {
            log(d)
        }
    }
}

fun printMatchingFiles(findFiles: List<FindFile>) {
    val files = findFiles.mapNotNull { f -> f.file }.map { f -> f.toString() }.distinct().sorted()
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
    val findFiles: List<FindFile> = finder.find()

    if (settings.listDirs) {
        printMatchingDirs(findFiles)
    }
    if (settings.listFiles) {
        printMatchingFiles(findFiles)
    }
}

fun main(args : Array<String>) {
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
