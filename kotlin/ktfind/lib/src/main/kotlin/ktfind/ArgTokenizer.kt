package ktfind

import org.json.JSONArray
import org.json.JSONException
import org.json.JSONObject
import org.json.JSONTokener
import java.io.FileNotFoundException
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Paths

enum class ArgTokenType {
    UNKNOWN, BOOL, STR, INT, LONG
}

data class ArgToken(val name: String, val type: ArgTokenType, val value: Any) {
}

class ArgTokenizer(options: List<Option>) {

    private var boolMap = mutableMapOf<String, String>()
    private var strMap = mutableMapOf<String, String>()
    private var intMap = mutableMapOf<String, String>()
    private var longMap = mutableMapOf<String, String>()

    private val longArgWithValRegex = Regex("^--([a-zA-Z0-9-]+)=(.*)$")
    private val longArgWithoutValRegex = Regex("^--([a-zA-Z0-9-]+)$")
    private val shortArgsRegex = Regex("^-([a-zA-Z0-9]{2,})$")
    private val shortArgRegex = Regex("^-([a-zA-Z0-9])$")

    init {
        options.forEach { option ->
            when (option.argType) {
                ArgTokenType.BOOL -> {
                    boolMap[option.longArg] = option.longArg
                    option.shortArg?.let { boolMap[it] = option.longArg }
                }

                ArgTokenType.STR -> {
                    strMap[option.longArg] = option.longArg
                    option.shortArg?.let { strMap[it] = option.longArg }
                }

                ArgTokenType.INT -> {
                    intMap[option.longArg] = option.longArg
                    option.shortArg?.let { intMap[it] = option.longArg }
                }

                ArgTokenType.LONG -> {
                    longMap[option.longArg] = option.longArg
                    option.shortArg?.let {
                        longMap[it] = option.longArg
                    }
                }

                else -> {
                    throw FindException("Invalid option: ${option.longArg}")
                }
            }
        }
    }

    private fun updateArgTokens(argName: String, argVal: Any?, argTokens: List<ArgToken>): List<ArgToken> {
        val nextArgTokens: List<ArgToken> =
            if (boolMap.containsKey(argName)) {
                when (argVal) {
                    null -> {
                        listOf(ArgToken(boolMap[argName]!!, ArgTokenType.BOOL, true))
                    }
                    is Boolean -> {
                        listOf(ArgToken(boolMap[argName]!!, ArgTokenType.BOOL, argVal))
                    }
                    else -> {
                        throw FindException("Invalid value for option: $argName")
                    }
                }
            } else if (strMap.containsKey(argName)) {
                when (argVal) {
                    null -> {
                        throw FindException("Missing value for option: $argName")
                    }
                    is String -> {
                        listOf(ArgToken(strMap[argName]!!, ArgTokenType.STR, argVal))
                    }
                    is Collection<*> -> {
                        if (argVal.any({ v -> v !is String })) {
                            throw FindException("Invalid value for option: $argName")
                        }
                        argVal.map { v -> ArgToken(strMap[argName]!!, ArgTokenType.STR, v as String) }.toList()
                    }
                    is JSONArray -> {
                        if (argVal.any({ v -> v !is String })) {
                            throw FindException("Invalid value for option: $argName")
                        }
                        argVal.map { v -> ArgToken(strMap[argName]!!, ArgTokenType.STR, v as String) }.toList()
                    }
                    else -> {
                        throw FindException("Invalid value for option: $argName")
                    }
                }
            } else if (intMap.containsKey(argName)) {
                when (argVal) {
                    null -> {
                        throw FindException("Missing value for option: $argName")
                    }
                    is Int -> {
                        listOf(ArgToken(intMap[argName]!!, ArgTokenType.INT, argVal))
                    }
                    is Long -> {
                        listOf(ArgToken(intMap[argName]!!, ArgTokenType.INT, argVal.toInt()))
                    }
                    is String -> {
                        listOf(ArgToken(intMap[argName]!!, ArgTokenType.INT, argVal.toInt()))
                    }
                    else -> {
                        throw FindException("Invalid value for option: $argName")
                    }
                }
            } else if (longMap.containsKey(argName)) {
                when (argVal) {
                    null -> {
                        throw FindException("Missing value for option: $argName")
                    }
                    is Long -> {
                        listOf(ArgToken(longMap[argName]!!, ArgTokenType.LONG, argVal))
                    }
                    is Int -> {
                        listOf(ArgToken(longMap[argName]!!, ArgTokenType.LONG, argVal.toLong()))
                    }
                    is String -> {
                        listOf(ArgToken(intMap[argName]!!, ArgTokenType.INT, argVal.toLong()))
                    }
                    else -> {
                        throw FindException("Invalid value for option: $argName")
                    }
                }
            } else {
                throw FindException("Invalid option: $argName")
            }
        return argTokens + nextArgTokens
    }

    fun tokenizeArgs(args: Array<String>): List<ArgToken> {
        fun recArgTokensFromArgs(args: List<String>, argTokens: List<ArgToken>): List<ArgToken> {
            if (args.isEmpty()) return argTokens
            val nextArg = args.first()
            if (nextArg matches longArgWithValRegex) {
                val match = longArgWithValRegex.matchEntire(nextArg)
                val longArg = match!!.groupValues[1]
                val argVal = match.groupValues[2]
                return recArgTokensFromArgs(args.drop(1), updateArgTokens(longArg, argVal, argTokens))
            }
            if (nextArg matches longArgWithoutValRegex) {
                val match = longArgWithoutValRegex.matchEntire(nextArg)
                val longArg = match!!.groupValues[1]
                val argVal: String? =
                    if ((strMap.containsKey(longArg)
                                || intMap.containsKey(longArg)
                                || longMap.containsKey(longArg))
                        && args.size > 1) {
                        args[1]
                    } else {
                        null
                    }
                val nextArgs =
                    if (argVal != null) {
                        args.drop(2)
                    } else {
                        args.drop(1)
                    }
                return recArgTokensFromArgs(nextArgs, updateArgTokens(longArg, argVal, argTokens))
            }
            if (nextArg matches shortArgsRegex) {
                val match = shortArgsRegex.matchEntire(nextArg)
                val shortArgs = match!!.groupValues[1]
                val shortArgList = shortArgs.map { c -> "-$c" }
                val nextArgs = shortArgList + args.drop(1)
                return recArgTokensFromArgs(nextArgs, argTokens)
            }
            if (nextArg matches shortArgRegex) {
                val match = shortArgRegex.matchEntire(nextArg)
                val shortArg = match!!.groupValues[1]
                val longArg: String? =
                    if (boolMap.containsKey(shortArg)) boolMap[shortArg]
                    else if (strMap.containsKey(shortArg)) strMap[shortArg]
                    else if (intMap.containsKey(shortArg)) intMap[shortArg]
                    else if (longMap.containsKey(shortArg)) longMap[shortArg]
                    else null
                if (longArg == null) {
                    throw FindException("Invalid option: $shortArg")
                }
                val argVal: String? =
                    if ((strMap.containsKey(longArg)
                                || intMap.containsKey(longArg)
                                || longMap.containsKey(longArg))
                        && args.size > 1) {
                        args[1]
                    } else {
                        null
                    }
                val nextArgs =
                    if (argVal != null) {
                        args.drop(2)
                    } else {
                        args.drop(1)
                    }
                return recArgTokensFromArgs(nextArgs, updateArgTokens(longArg, argVal, argTokens))
            }
            return recArgTokensFromArgs(args.drop(1), argTokens + ArgToken("path", ArgTokenType.STR, nextArg))
        }
        return recArgTokensFromArgs(args.toList(), listOf())
    }

    fun tokenizeJson(json: String): List<ArgToken> {
        fun recTokenizeJson(keys: List<String>, jsonObject: JSONObject, argTokens: List<ArgToken>): List<ArgToken> {
            return if (keys.isEmpty()) argTokens
            else {
                val k = keys.first()
                val v = jsonObject.get(k)
                recTokenizeJson(keys.drop(1), jsonObject, updateArgTokens(k, v, argTokens))
            }
        }
        try {
            val jsonObject = JSONObject(JSONTokener(json))
            // keys are sorted so that output is consistent across all versions
            val keys = jsonObject.keySet().toList().sorted()
            return recTokenizeJson(keys, jsonObject, listOf())
        } catch (_: JSONException) {
            throw FindException("Unable to parse JSON settings")
        }
    }

    fun tokenizeFile(filePath: String): List<ArgToken> {
        val path = FileUtil.expandPath(Paths.get(filePath));
        if (!Files.exists(path)) {
            throw FindException("Settings file not found: $filePath")
        }
        if (!filePath.endsWith(".json")) {
            throw FindException("Invalid settings file (must be JSON): $filePath")
        }
        try {
            val json = path.toFile().readText()
            return tokenizeJson(json)
        } catch (_: FileNotFoundException) {
            throw FindException("Settings file not found: $filePath")
        } catch (_: IOException) {
            throw FindException("IOException reading settings file: $filePath")
        } catch (_: JSONException) {
            throw FindException("Unable to parse JSON in settings file: $filePath")
        }
    }
}
