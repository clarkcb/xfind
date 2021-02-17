package ktfind

import java.util.Comparator

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

class FindResultComparator : Comparator<FindResult> {
    override fun compare(r1: FindResult, r2: FindResult): Int {
        val pathCmp = (r1.file!!.file.parent ?: "").toLowerCase().
                compareTo((r2.file!!.file.parent ?: "").toLowerCase())
        if (pathCmp == 0) {
            val fileCmp = r1.file.file.name.toLowerCase().
                    compareTo(r2.file.file.name.toLowerCase())
            if (fileCmp == 0) {
                val lineNumCmp = signum(r1.lineNum - r2.lineNum)
                if (lineNumCmp == 0) {
                    return signum(r1.matchStartIndex- r2.matchStartIndex)
                }
                return lineNumCmp
            }
            return fileCmp
        }
        return pathCmp
    }
}

fun printResults(results: List<FindResult>, settings: FindSettings) {
    log("\nFind results (${results.size}):")
    val formatter = FindResultFormatter(settings)
    for (r in results.sortedWith(FindResultComparator())) {
        log(formatter.format(r))
    }
}

fun printMatchingDirs(results: List<FindResult>) {
    val dirs = results.mapNotNull { r -> r.file }.mapNotNull { f -> f.file.parent }.distinct().sorted()
    log("\nDirectories with matches (${dirs.size}):")
    for (d in dirs) {
        log(d)
    }
}

fun printMatchingFiles(results: List<FindResult>) {
    val files = results.mapNotNull { r -> r.file }.map { f -> f.toString() }.distinct().sorted()
    log("\nFiles with matches (${files.size}):")
    for (f in files) {
        log(f)
    }
}

fun printMatchingLines(settings: FindSettings, results: List<FindResult>) {
    val lines: List<String> =
            if (settings.uniqueLines) results.map { r -> r.line.trim() }.
                    distinct().sorted()
            else results.map { r -> r.line.trim() }.sorted()
    val hdr =
            if (settings.uniqueLines) "\nUnique lines with matches (${lines.size}):"
            else "\nLines with matches (${lines.size}):"
    log(hdr)
    for (l in lines) {
        log(l)
    }
}

fun find(settings: FindSettings) {
    val finder = Finder(settings)
    val results: List<FindResult> = finder.find()

    if (settings.printResults) {
        printResults(results, settings)
    }
    if (settings.listDirs) {
        printMatchingDirs(results)
    }
    if (settings.listFiles) {
        printMatchingFiles(results)
    }
    if (settings.listLines) {
        printMatchingLines(settings, results)
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
