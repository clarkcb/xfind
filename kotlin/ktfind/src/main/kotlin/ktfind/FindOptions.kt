package ktfind

import org.json.JSONArray
import org.json.JSONObject
import org.json.JSONTokener
import java.io.File
import java.io.FileNotFoundException
import java.io.IOException

/**
 * @author cary on 7/23/16.
 */
data class FindOption(val shortArg: String?, val longArg: String, val desc: String) {
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
    private var longArgMap = mutableMapOf<String, String>()

    init {
        findOptions = loadFindOptionsFromJson()
    }

    private fun loadFindOptionsFromJson(): List<FindOption> {
        val findOptionsInputStream = javaClass.getResourceAsStream(findOptionsJsonPath)
        val jsonObj = JSONObject(JSONTokener(findOptionsInputStream))
        val findOptionsArray = jsonObj.getJSONArray("findoptions").iterator()
        val options: MutableList<FindOption> = mutableListOf()
        while (findOptionsArray.hasNext()) {
            val findOptionObj = findOptionsArray.next() as JSONObject
            val longArg = findOptionObj.getString("long")
            longArgMap[longArg] = longArg
            val shortArg =
                if (findOptionObj.has("short")) {
                    val sArg = findOptionObj.getString("short")
                    longArgMap[sArg] = longArg
                    sArg
                } else {
                    null
                }
            val desc = findOptionObj.getString("desc")
            options.add(FindOption(shortArg, longArg, desc))
        }
        return options.toList().sortedBy { it.sortArg }
    }

    private val boolActionMap: Map<String, ((Boolean, FindSettings) -> FindSettings)> = mapOf(
        "archivesonly" to { b, ss ->
            if (b) ss.copy(
                archivesOnly = b,
                includeArchives = b
            ) else ss.copy(archivesOnly = b)
        },
        "debug" to { b, ss ->
            if (b) ss.copy(debug = b, verbose = b) else
                ss.copy(debug = b)
        },
        "excludearchives" to { b, ss -> ss.copy(includeArchives = !b) },
        "excludehidden" to { b, ss -> ss.copy(includeHidden = !b) },
        "followsymlinks" to { b, ss -> ss.copy(followSymlinks = b) },
        "help" to { b, ss -> ss.copy(printUsage = b) },
        "includearchives" to { b, ss -> ss.copy(includeArchives = b) },
        "includehidden" to { b, ss -> ss.copy(includeHidden = b) },
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
                { s, ss -> settingsFromFile(s, ss) },
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

    private fun settingsFromFile(filePath: String, settings: FindSettings): FindSettings {
        val file = File(filePath)
        try {
            val json = file.readText()
            return settingsFromJson(json, settings)
        } catch (_: FileNotFoundException) {
            throw FindException("Settings file not found: $filePath")
        } catch (_: IOException) {
            throw FindException("IOException reading settings file: $filePath")
        }
    }

    fun settingsFromJson(json: String, settings: FindSettings): FindSettings {
        val jsonObject = JSONObject(JSONTokener(json))
        fun recSettingsFromJson(keys: List<String>, settings: FindSettings): FindSettings {
            return if (keys.isEmpty()) settings
            else {
                val ko = keys.first()
                val vo = jsonObject.get(ko)
                if (vo != null) {
                    recSettingsFromJson(keys.drop(1), applySetting(ko, vo, settings))
                } else {
                    recSettingsFromJson(keys.drop(1), settings)
                }
            }
        }
        return recSettingsFromJson(jsonObject.keySet().toList(), settings)
    }

    private fun applySetting(key: String, obj: Any, settings: FindSettings): FindSettings {
        when (obj) {
            is Boolean -> {
                return applyBoolSetting(key, obj, settings)
            }
            is String -> {
                return applyStringSetting(key, obj, settings)
            }
            is Int -> {
                return applyIntSetting(key, obj, settings)
            }
            is Long -> {
                return applyLongSetting(key, obj, settings)
            }
            is JSONArray -> {
                return applySettings(key, obj.toList().map { it as String }, settings)
            }
            else -> {
                return settings
            }
        }
    }

    private fun applyBoolSetting(key: String, bool: Boolean, settings: FindSettings): FindSettings {
        if (this.boolActionMap.containsKey(key)) {
            return this.boolActionMap[key]!!.invoke(bool, settings)
        } else {
            throw FindException("Invalid option: $key")
        }
    }

    private fun applyStringSetting(key: String, s: String, settings: FindSettings): FindSettings {
        return when {
            this.stringActionMap.containsKey(key) -> {
                this.stringActionMap[key]!!.invoke(s, settings)
            }
            else -> {
                throw FindException("Invalid option: $key")
            }
        }
    }

    private fun applyIntSetting(key: String, i: Int, settings: FindSettings): FindSettings {
        return if (this.intActionMap.containsKey(key)) {
            this.intActionMap[key]!!.invoke(i, settings)
        } else if (this.longActionMap.containsKey(key)) {
            this.longActionMap[key]!!.invoke(i.toLong(), settings)
        } else {
            throw FindException("Invalid option: $key")
        }
    }

    private fun applyLongSetting(key: String, l: Long, settings: FindSettings): FindSettings {
        return if (this.intActionMap.containsKey(key)) {
            this.intActionMap[key]!!.invoke(l.toInt(), settings)
        } else if (this.longActionMap.containsKey(key)) {
            this.longActionMap[key]!!.invoke(l, settings)
        } else {
            throw FindException("Invalid option: $key")
        }
    }

    private fun applySettings(key: String, lst: List<String>, settings: FindSettings): FindSettings {
        return if (lst.isEmpty()) settings
        else {
            applySettings(key, lst.drop(1), applySetting(key, lst.first(), settings))
        }
    }

    fun settingsFromArgs(args: Array<String>): FindSettings {
        fun recSettingsFromArgs(args: List<String>, settings: FindSettings): FindSettings {
            if (args.isEmpty()) return settings
            val nextArg = args.first()
            if (nextArg.startsWith("-")) {
                val arg = nextArg.dropWhile { it == '-' }
                val longArg = longArgMap[arg]
                return if (boolActionMap.containsKey(longArg)) {
                    val ss = boolActionMap[longArg]!!.invoke(true, settings)
                    recSettingsFromArgs(args.drop(1), ss)
                } else if (stringActionMap.containsKey(longArg)
                    || intActionMap.containsKey(longArg)
                    || longActionMap.containsKey(longArg)) {
                    if (args.size > 1) {
                        val argVal = args.drop(1).first()
                        val ss = if (stringActionMap.containsKey(longArg)) {
                            stringActionMap[longArg]!!.invoke(argVal, settings)
                        } else if (intActionMap.containsKey(longArg)) {
                            intActionMap[longArg]!!.invoke(argVal.toInt(), settings)
                        } else if (longActionMap.containsKey(longArg)) {
                            longActionMap[longArg]!!.invoke(argVal.toLong(), settings)
                        } else {
                            throw FindException("Unhandled option $arg")
                        }
                        recSettingsFromArgs(args.drop(2), ss)
                    } else {
                        throw FindException("Missing value for option $arg")
                    }
                } else {
                    throw FindException("Invalid option: $arg")
                }
            } else {
                return recSettingsFromArgs(args.drop(1), settings.copy(paths = addPath(nextArg, settings.paths)))
            }
        }
        // default printFiles to true since running as cli
        return recSettingsFromArgs(args.toList(), getDefaultSettings().copy(printFiles = true))
    }

    fun usage() {
        log(getUsageString())
    }

    private fun getUsageString(): String {
        val sb = StringBuilder()
        sb.append("Usage:\n")
        sb.append(" ktfind [options] <path> [<path> ...]\n\n")
        sb.append("Options:\n")
        fun getOptString(so: FindOption): String {
            return (if (so.shortArg == null) "" else "-${so.shortArg},") + "--${so.longArg}"
        }

        val optPairs = findOptions.map { Pair(getOptString(it), it.desc) }
        val longest = optPairs.maxOfOrNull { it.first.length }
        val format = " %1${'$'}-${longest}s  %2${'$'}s\n"
        for (o in optPairs) {
            sb.append(String.format(format, o.first, o.second))
        }
        return sb.toString()
    }
}
