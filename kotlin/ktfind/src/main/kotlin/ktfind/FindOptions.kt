package ktfind

import org.json.JSONArray
import org.json.JSONObject
import org.json.JSONTokener
import java.io.File
import java.io.FileNotFoundException
import java.io.IOException
import java.nio.file.Path
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
            val shortArg =
                if (findOptionObj.has("short")) {
                    findOptionObj.getString("short")
                } else {
                    null
                }
            val desc = findOptionObj.getString("desc")
            options.add(FindOption(shortArg, longArg, desc))
        }
        return options.toList().sortedBy { it.sortArg }
    }

    private fun getArgMap(): Map<String, String> {
        val longOpts = findOptions.map { Pair(it.longArg, it.longArg) }.toMap()
        val shortOpts = findOptions.filter { it.shortArg != null }.map { Pair(it.shortArg!!, it.longArg) }.toMap()
        return longOpts.plus(shortOpts)
    }

    private val argActionMap: Map<String, ((String, FindSettings) -> FindSettings)> = mapOf(
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
        "maxdepth" to
                { s, ss -> ss.copy(maxDepth = Integer.parseInt(s)) },
        "maxlastmod" to
                { s, ss -> ss.copy(maxLastMod = getLastModFromString(s)) },
        "maxsize" to
                { s, ss -> ss.copy(maxSize = Integer.parseInt(s)) },
        "mindepth" to
                { s, ss -> ss.copy(minDepth = Integer.parseInt(s)) },
        "minlastmod" to
                { s, ss -> ss.copy(minLastMod = getLastModFromString(s)) },
        "minsize" to
                { s, ss -> ss.copy(minSize = Integer.parseInt(s)) },
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

    private val boolFlagActionMap: Map<String, ((Boolean, FindSettings) -> FindSettings)> = mapOf(
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
        "help" to { b, ss -> ss.copy(printUsage = b) },
        "includearchives" to { b, ss -> ss.copy(includeArchives = b) },
        "includehidden" to { b, ss -> ss.copy(includeHidden = b) },
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

    private fun settingsFromFile(filePath: String, settings: FindSettings): FindSettings {
        val file = File(filePath)
        try {
            val json = file.readText()
            return settingsFromJson(json, settings)
        } catch (e: FileNotFoundException) {
            throw FindException("Settings file not found: $filePath")
        } catch (e: IOException) {
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
            is String -> {
                return applySetting(key, obj, settings)
            }

            is Boolean -> {
                return applySetting(key, obj, settings)
            }

            is Int -> {
                return applySetting(key, obj.toString(), settings)
            }

            is Long -> {
                return applySetting(key, obj.toString(), settings)
            }

            is JSONArray -> {
                return applySetting(key, obj.toList().map { it as String }, settings)
            }

            else -> {
                return settings
            }
        }
    }

    private fun applySetting(key: String, s: String, settings: FindSettings): FindSettings {
        return when {
            this.argActionMap.containsKey(key) -> {
                this.argActionMap[key]!!.invoke(s, settings)
            }

            key == "path" -> {
                settings.copy(paths = settings.paths.plus(s))
            }

            else -> {
                throw FindException("Invalid option: $key")
            }
        }
    }

    private fun applySetting(key: String, bool: Boolean, settings: FindSettings): FindSettings {
        if (this.boolFlagActionMap.containsKey(key)) {
            return this.boolFlagActionMap[key]!!.invoke(bool, settings)
        } else {
            throw FindException("Invalid option: $key")
        }
    }

    private fun applySetting(key: String, lst: List<String>, settings: FindSettings): FindSettings {
        return if (lst.isEmpty()) settings
        else {
            applySetting(key, lst.drop(1), applySetting(key, lst.first(), settings))
        }
    }

    fun settingsFromArgs(args: Array<String>): FindSettings {
        val argMap = getArgMap()
        fun recSettingsFromArgs(args: List<String>, settings: FindSettings): FindSettings {
            if (args.isEmpty()) return settings
            val nextArg = args.first()
            if (nextArg.startsWith("-")) {
                val arg = nextArg.dropWhile { it == '-' }
                if (argMap.containsKey(arg)) {
                    val longArg = argMap[arg]
                    return if (argActionMap.containsKey(longArg)) {
                        if (args.size > 1) {
                            val argVal = args.drop(1).first()
                            val ss = argActionMap[longArg]!!.invoke(argVal, settings)
                            recSettingsFromArgs(args.drop(2), ss)
                        } else {
                            throw FindException("Missing value for option $arg")
                        }
                    } else if (boolFlagActionMap.containsKey(longArg)) {
                        val ss = boolFlagActionMap[longArg]!!.invoke(true, settings)
                        recSettingsFromArgs(args.drop(1), ss)
                    } else {
                        throw FindException("Invalid option: $arg")
                    }
                } else {
                    throw FindException("Invalid option: $arg")
                }
            } else {
                return recSettingsFromArgs(args.drop(1), settings.copy(paths = settings.paths.plus(nextArg)))
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
        val longest = optPairs.map { it.first.length }.maxOrNull()
        val format = " %1${'$'}-${longest}s  %2${'$'}s\n"
        for (o in optPairs) {
            sb.append(String.format(format, o.first, o.second))
        }
        return sb.toString()
    }
}
