package ktfind

import org.json.JSONArray
import org.json.JSONException
import org.json.JSONObject
import org.json.JSONTokener
import java.io.FileNotFoundException
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Paths

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
    // We add path manually since it's not an option in findoptions.json
    private var longArgMap = mutableMapOf<String, String>("path" to "path")

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
            if (b) ss.copy(archivesOnly = true, includeArchives = true) else
                ss.copy(archivesOnly = false)
        },
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
                { s, ss -> updateSettingsFromFile(s, ss) },
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

    private fun applySettings(key: String, lst: List<Any>, settings: FindSettings): FindSettings {
        return if (lst.isEmpty()) settings
        else {
            applySettings(key, lst.drop(1), applySetting(key, lst.first(), settings))
        }
    }

    private fun applySetting(key: String, obj: Any, settings: FindSettings): FindSettings {
        if (boolActionMap.containsKey(key)) {
            if (obj is Boolean) {
                return this.boolActionMap[key]!!.invoke(obj, settings)
            } else {
                throw FindException("Invalid value for option: $key")
            }
        } else if (stringActionMap.containsKey(key)) {
            return if (obj is String) {
                this.stringActionMap[key]!!.invoke(obj, settings)
            } else if (obj is JSONArray) {
                applySettings(key, obj.toList(), settings)
            } else {
                throw FindException("Invalid value for option: $key")
            }
        } else if (intActionMap.containsKey(key)) {
            return if (obj is Int) {
                this.intActionMap[key]!!.invoke(obj, settings)
            } else if (obj is Long) {
                this.intActionMap[key]!!.invoke(obj.toInt(), settings)
            } else {
                throw FindException("Invalid value for option: $key")
            }
        } else if (longActionMap.containsKey(key)) {
            return if (obj is Int) {
                this.longActionMap[key]!!.invoke(obj.toLong(), settings)
            } else if (obj is Long) {
                this.longActionMap[key]!!.invoke(obj, settings)
            } else {
                throw FindException("Invalid value for option: $key")
            }
        } else {
            throw FindException("Invalid option: $key")
        }
    }

    fun updateSettingsFromJson(json: String, settings: FindSettings): FindSettings {
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
        // keys are sorted so that output is consistent across all versions
        val keys = jsonObject.keySet().toList().sorted()
        val invalidKeys = keys.filter { !longArgMap.containsKey(it) }
        if (invalidKeys.isNotEmpty()) {
            throw FindException("Invalid option: ${invalidKeys[0]}")
        }
        return recSettingsFromJson(keys, settings)
    }

    private fun updateSettingsFromFile(filePath: String, settings: FindSettings): FindSettings {
        val path = FileUtil.expandPath(Paths.get(filePath));
        if (!Files.exists(path)) {
            throw FindException("Settings file not found: $filePath")
        }
        if (!filePath.endsWith(".json")) {
            throw FindException("Invalid settings file (must be JSON): $filePath")
        }
        try {
            val json = path.toFile().readText()
            return updateSettingsFromJson(json, settings)
        } catch (_: FileNotFoundException) {
            throw FindException("Settings file not found: $filePath")
        } catch (_: IOException) {
            throw FindException("IOException reading settings file: $filePath")
        } catch (_: JSONException) {
            throw FindException("Unable to parse JSON in settings file: $filePath")
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
