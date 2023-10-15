package ktfind

import java.io.File
import java.io.IOException

import org.json.JSONObject
import org.json.JSONTokener

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
//private const val searchable = "searchable"
private const val text = "text"
private const val xml = "xml"
//private const val unknown = "unknown"

fun fileTypeFromName(name: String) : FileType {
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
            val jsonObj = JSONObject(JSONTokener(fileTypesInputStream!!))
            val fileTypesArray = jsonObj.getJSONArray("filetypes")!!.iterator()
            while (fileTypesArray.hasNext()) {
                val it = fileTypesArray.next() as JSONObject
                val typeName = it.getString("type")!!
                fileTypeExtMap[typeName] = it.getJSONArray("extensions")!!.toList().map { it.toString() }.toSet()
                fileTypeNameMap[typeName] = it.getJSONArray("names")!!.toList().map { it.toString() }.toSet()
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
