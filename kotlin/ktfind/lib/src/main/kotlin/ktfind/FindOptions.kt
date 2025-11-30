package ktfind

import org.json.JSONObject
import org.json.JSONTokener

/**
 * @author cary on 7/23/16.
 */
interface Option {
    val shortArg: String?
    val longArg: String
    val desc: String
    val argType: ArgTokenType
}

data class FindOption(override val shortArg: String?, override val longArg: String, override val desc: String,
                      override val argType: ArgTokenType) : Option {
    val sortArg =
        if (shortArg == null) {
            longArg.lowercase()
        } else {
            shortArg.lowercase() + "@" + longArg.lowercase()
        }
}

class FindOptions {
    private val findOptionsJsonPath = "/findoptions.json"
    private val findOptions: List<FindOption>
    // We add path manually since it's not an option in findoptions.json
    private var longArgs = mutableSetOf<String>("path")
    private var argTokenizer: ArgTokenizer

    private val boolActionMap: Map<String, ((Boolean, FindSettings) -> FindSettings)> = mapOf(
        "archivesonly" to { b, ss ->
            if (b) ss.copy(archivesOnly = true, includeArchives = true) else
                ss.copy(archivesOnly = false)
        },
        "colorize" to { b, ss -> ss.copy(colorize = b) },
        "debug" to { b, ss ->
            if (b) ss.copy(debug = true, verbose = true) else
                ss.copy(debug = false)
        },
        "excludearchives" to { b, ss -> ss.copy(includeArchives = !b) },
        "excludehidden" to { b, ss -> ss.copy(includeHidden = !b) },
        "followsymlinks" to { b, ss -> ss.copy(followSymlinks = b) },
        "help" to { b, ss -> ss.copy(printUsage = b) },
        "includearchives" to { b, ss -> ss.copy(includeArchives = b) },
        "includehidden" to { b, ss -> ss.copy(includeHidden = b) },
        "nocolorize" to { b, ss -> ss.copy(colorize = !b) },
        "nofollowsymlinks" to { b, ss -> ss.copy(followSymlinks = !b) },
        "noprintdirs" to { b, ss -> ss.copy(printDirs = !b) },
        "noprintfiles" to { b, ss -> ss.copy(printFiles = !b) },
        "norecursive" to { b, ss -> ss.copy(recursive = !b) },
        "printdirs" to { b, ss -> ss.copy(printDirs = b) },
        "printfiles" to { b, ss -> ss.copy(printFiles = b) },
        "recursive" to { b, ss -> ss.copy(recursive = b) },
        "sort-ascending" to { b, ss -> ss.copy(sortDescending = !b) },
        "sort-caseinsensitive" to { b, ss -> ss.copy(sortCaseInsensitive = b) },
        "sort-casesensitive" to { b, ss -> ss.copy(sortCaseInsensitive = !b) },
        "sort-descending" to { b, ss -> ss.copy(sortDescending = b) },
        "verbose" to { b, ss -> ss.copy(verbose = b) },
        "version" to { b, ss -> ss.copy(printVersion = b) }
    )

    private val stringActionMap: Map<String, ((String, FindSettings) -> FindSettings)> = mapOf(
        "in-archiveext" to
                { s, ss -> ss.copy(inArchiveExtensions = addExtensions(s, ss.inArchiveExtensions)) },
        "in-archivefilepattern" to
                { s, ss -> ss.copy(inArchiveFilePatterns = ss.inArchiveFilePatterns.plus(Regex(s))) },
        "in-dirpattern" to
                { s, ss -> ss.copy(inDirPatterns = ss.inDirPatterns.plus(Regex(s))) },
        "in-ext" to
                { s, ss -> ss.copy(inExtensions = addExtensions(s, ss.inExtensions)) },
        "in-filepattern" to
                { s, ss -> ss.copy(inFilePatterns = ss.inFilePatterns.plus(Regex(s))) },
        "in-filetype" to
                { s, ss -> ss.copy(inFileTypes = addFileTypes(s, ss.inFileTypes)) },
        "maxlastmod" to
                { s, ss -> ss.copy(maxLastMod = getLastModFromString(s)) },
        "minlastmod" to
                { s, ss -> ss.copy(minLastMod = getLastModFromString(s)) },
        "out-archiveext" to
                { s, ss -> ss.copy(outArchiveExtensions = addExtensions(s, ss.outArchiveExtensions)) },
        "out-archivefilepattern" to
                { s, ss -> ss.copy(outArchiveFilePatterns = ss.outArchiveFilePatterns.plus(Regex(s))) },
        "out-dirpattern" to
                { s, ss -> ss.copy(outDirPatterns = ss.outDirPatterns.plus(Regex(s))) },
        "out-ext" to
                { s, ss -> ss.copy(outExtensions = addExtensions(s, ss.outExtensions)) },
        "out-filepattern" to
                { s, ss -> ss.copy(outFilePatterns = ss.outFilePatterns.plus(Regex(s))) },
        "out-filetype" to
                { s, ss -> ss.copy(outFileTypes = addFileTypes(s, ss.outFileTypes)) },
        "path" to
                { s, ss -> ss.copy(paths = addPath(s, ss.paths)) },
        "settings-file" to
                { s, ss -> updateSettingsFromFile(ss, s) },
        "sort-by" to
                { s, ss -> ss.copy(sortBy = SortBy.forName(s)) },
    )

    private val intActionMap: Map<String, ((Int, FindSettings) -> FindSettings)> = mapOf(
        "maxdepth" to { i, ss -> ss.copy(maxDepth = i) },
        "mindepth" to { i, ss -> ss.copy(minDepth = i) }
    )

    private val longActionMap: Map<String, ((Long, FindSettings) -> FindSettings)> = mapOf(
        "maxsize" to { l, ss -> ss.copy(maxSize = l) },
        "minsize" to { l, ss -> ss.copy(minSize = l) }
    )

    private fun loadFindOptionsFromJson(): List<FindOption> {
        val findOptionsInputStream = javaClass.getResourceAsStream(findOptionsJsonPath)
        val jsonObj = JSONObject(JSONTokener(findOptionsInputStream))
        val findOptionsArray = jsonObj.getJSONArray("findoptions").iterator()
        val options: MutableList<FindOption> = mutableListOf()
        while (findOptionsArray.hasNext()) {
            val findOptionObj = findOptionsArray.next() as JSONObject
            val longArg = findOptionObj.getString("long")
            longArgs.add(longArg)
            val shortArg =
                if (findOptionObj.has("short")) {
                    val sArg = findOptionObj.getString("short")
                    sArg
                } else {
                    null
                }
            val desc = findOptionObj.getString("desc")
            var argType = ArgTokenType.UNKNOWN
            if (boolActionMap.containsKey(longArg)) {
                argType = ArgTokenType.BOOL
            } else if (stringActionMap.containsKey(longArg)) {
                argType = ArgTokenType.STR
            } else if (intActionMap.containsKey(longArg)) {
                argType = ArgTokenType.INT
            } else if (longActionMap.containsKey(longArg)) {
                argType = ArgTokenType.LONG
            }
            options.add(FindOption(shortArg, longArg, desc, argType))
        }
        return options.toList()
    }

    init {
        findOptions = loadFindOptionsFromJson()
        argTokenizer = ArgTokenizer(findOptions)
    }

    private fun applyArgTokenToSettings(argToken: ArgToken, settings: FindSettings): FindSettings {
        if (argToken.type == ArgTokenType.BOOL) {
            if (argToken.value is Boolean) {
                return this.boolActionMap[argToken.name]!!.invoke(argToken.value, settings)
            } else {
                throw FindException("Invalid value for option: ${argToken.name}")
            }
        } else if (argToken.type == ArgTokenType.STR) {
            return when (argToken.value) {
                is String -> {
                    this.stringActionMap[argToken.name]!!.invoke(argToken.value, settings)
                }
                is Iterable<Any?> -> {
                    var updatedSettings: FindSettings = settings
                    for (v in argToken.value) {
                        if (v is String) {
                            updatedSettings = this.stringActionMap[argToken.name]!!.invoke(v, updatedSettings)
                        } else {
                            throw FindException("Invalid value for option: ${argToken.name}")
                        }
                    }
                    updatedSettings
                }
                else -> {
                    throw FindException("Invalid value for option: ${argToken.name}")
                }
            }
        } else if (argToken.type == ArgTokenType.INT) {
            return when (argToken.value) {
                is Int -> {
                    this.intActionMap[argToken.name]!!.invoke(argToken.value, settings)
                }
                is Long -> {
                    this.intActionMap[argToken.name]!!.invoke(argToken.value.toInt(), settings)
                }
                else -> {
                    throw FindException("Invalid value for option: ${argToken.name}")
                }
            }
        } else if (argToken.type == ArgTokenType.LONG) {
            return when (argToken.value) {
                is Int -> {
                    this.longActionMap[argToken.name]!!.invoke(argToken.value.toLong(), settings)
                }
                is Long -> {
                    this.longActionMap[argToken.name]!!.invoke(argToken.value, settings)
                }
                else -> {
                    throw FindException("Invalid value for option: ${argToken.name}")
                }
            }
        } else {
            throw FindException("Invalid option: ${argToken.name}")
        }
    }

    private fun updateSettingsFromArgTokens(settings: FindSettings, argTokens: List<ArgToken>): FindSettings {
        var updatedSettings: FindSettings = settings
        for (argToken in argTokens) {
            updatedSettings = applyArgTokenToSettings(argToken, updatedSettings)
        }
        return updatedSettings
    }

    fun updateSettingsFromJson(settings: FindSettings, json: String): FindSettings {
        val argTokens = argTokenizer.tokenizeJson(json)
        return updateSettingsFromArgTokens(settings, argTokens)
    }

    fun updateSettingsFromFile(settings: FindSettings, filePath: String): FindSettings {
        val argTokens = argTokenizer.tokenizeFile(filePath)
        return updateSettingsFromArgTokens(settings, argTokens)
    }

    fun updateSettingsFromArgs(settings: FindSettings, args: Array<String>): FindSettings {
        val argTokens = argTokenizer.tokenizeArgs(args)
        return updateSettingsFromArgTokens(settings, argTokens)
    }

    fun settingsFromArgs(args: Array<String>): FindSettings {
        if (args.isEmpty()) {
            throw FindException(FindError.STARTPATH_NOT_DEFINED.message)
        }
        val settings = getDefaultSettings().copy(printFiles = true)
        return updateSettingsFromArgs(settings, args)
    }

    private fun getUsageString(): String {
        val sb = StringBuilder()
        sb.append("Usage:\n")
        sb.append(" ktfind [options] <path> [<path> ...]\n\n")
        sb.append("Options:\n")
        fun getOptString(so: FindOption): String {
            return (if (so.shortArg == null) "" else "-${so.shortArg},") + "--${so.longArg}"
        }

        val optPairs = findOptions.sortedBy { it.sortArg }.map { Pair(getOptString(it), it.desc) }
        val longest = optPairs.maxOfOrNull { it.first.length }
        val format = $$" %1$-$${longest}s  %2$s\n"
        for (o in optPairs) {
            sb.append(String.format(format, o.first, o.second))
        }
        return sb.toString()
    }

    fun usage() {
        log(getUsageString())
    }
}
