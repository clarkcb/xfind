package ktfind

import org.json.simple.JSONArray
import org.json.simple.JSONObject
import org.json.simple.parser.JSONParser
import org.json.simple.parser.ParseException
import java.io.File
import java.io.IOException
import java.io.InputStreamReader

/**
 * @author cary on 7/24/16.
 */

enum class FileType {
    UNKNOWN,
    ARCHIVE,
    BINARY,
    CODE,
    TEXT,
    XML
}

private const val archive = "archive"
private const val code = "code"
private const val binary = "binary"
// private const val findable = "findable"
private const val text = "text"
private const val xml = "xml"
//private const val unknown = "unknown"

fun fromName(name: String) : FileType {
    when (name.trim().lowercase()) {
        text -> {
            return FileType.TEXT
        }
        binary -> {
            return FileType.BINARY
        }
        archive -> {
            return FileType.ARCHIVE
        }
        code -> {
            return FileType.CODE
        }
        xml -> {
            return FileType.XML
        }
        else -> {
            return FileType.UNKNOWN
        }
    }
}

private const val fileTypesJsonPath = "/filetypes.json"

class FileTypes {

    private val fileTypeExtMap: MutableMap<String, Set<String>> = mutableMapOf()
    private val fileTypeNameMap: MutableMap<String, Set<String>> = mutableMapOf()

    init {
        setFileTypeMapsFromJson()
    }

    private fun setFileTypeMapsFromJson() {
        try {
            val fileTypesInputStream = javaClass.getResourceAsStream(fileTypesJsonPath)
            val obj: Any = JSONParser().parse(InputStreamReader(fileTypesInputStream!!))
            val jsonObj = obj as JSONObject
            val filetypesArray = jsonObj["filetypes"] as JSONArray
            for (o in filetypesArray) {
                val filetypeMap = o as Map<*, *>
                val typeName = filetypeMap["type"] as String
                val extArray = filetypeMap["extensions"] as JSONArray
                val extSet: Set<String> = extArray.map { e -> e.toString() }.toSet()
                fileTypeExtMap[typeName] = extSet
                val nameArray = filetypeMap["names"] as JSONArray
                val nameSet: Set<String> = nameArray.map { e -> e.toString() }.toSet()
                fileTypeNameMap[typeName] = nameSet
            }
            val allTextExts: MutableSet<String> = mutableSetOf()
            allTextExts.addAll(fileTypeExtMap["code"]!!)
            allTextExts.addAll(fileTypeExtMap["text"]!!)
            allTextExts.addAll(fileTypeExtMap["xml"]!!)
            fileTypeExtMap["text"] = allTextExts
            val allTextNames: MutableSet<String> = mutableSetOf()
            allTextNames.addAll(fileTypeNameMap["code"]!!)
            allTextNames.addAll(fileTypeNameMap["text"]!!)
            allTextNames.addAll(fileTypeNameMap["xml"]!!)
            fileTypeNameMap["text"] = allTextNames
            // val allFindable: MutableSet<String> = mutableSetOf()
            // allFindable.addAll(fileTypeExtMap["archive"]!!)
            // allFindable.addAll(fileTypeExtMap["binary"]!!)
            // allFindable.addAll(fileTypeExtMap["text"]!!)
            // fileTypeExtMap["findable"] = allFindable
        } catch (e: ParseException) {
            e.printStackTrace()
        } catch (e: IOException) {
            e.printStackTrace()
        }
    }

    fun getFileType(file: File) : FileType {
        when {
            isCodeFile(file) -> {
                return FileType.CODE
            }
            isXmlFile(file) -> {
                return FileType.XML
            }
            isTextFile(file) -> {
                return FileType.TEXT
            }
            isBinaryFile(file) -> {
                return FileType.BINARY
            }
            isArchiveFile(file) -> {
                return FileType.ARCHIVE
            }
            else -> {
                return FileType.UNKNOWN
            }
        }
    }

    fun isArchiveFile(file: File): Boolean {
        return (fileTypeNameMap[archive] ?: setOf()).contains(file.name)
                || (fileTypeExtMap[archive] ?: setOf()).contains(file.extension.lowercase())
    }

    fun isBinaryFile(file: File): Boolean {
        return (fileTypeNameMap[binary] ?: setOf()).contains(file.name)
                || (fileTypeExtMap[binary] ?: setOf()).contains(file.extension.lowercase())
    }

    fun isCodeFile(file: File): Boolean {
        return (fileTypeNameMap[code] ?: setOf()).contains(file.name)
                || (fileTypeExtMap[code] ?: setOf()).contains(file.extension.lowercase())
    }

    // fun isFindableFile(file: File): Boolean {
    //     return (fileTypeExtMap[findable] ?: setOf()).contains(file.extension.lowercase())
    // }

    fun isTextFile(file: File): Boolean {
        return (fileTypeNameMap[text] ?: setOf()).contains(file.name)
                || (fileTypeExtMap[text] ?: setOf()).contains(file.extension.lowercase())
    }

    fun isUnknownFile(file: File): Boolean {
        return getFileType(file) == FileType.UNKNOWN
    }

    fun isXmlFile(file: File): Boolean {
        return (fileTypeNameMap[xml] ?: setOf()).contains(file.name)
                || (fileTypeExtMap[xml] ?: setOf()).contains(file.extension.lowercase())
    }
}
