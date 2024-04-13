package ktfind

import java.io.File
import java.io.IOException

import org.json.JSONObject
import org.json.JSONTokener

/**
 * @author cary on 7/24/16.
 */

enum class FileType(val value: String) {
    UNKNOWN("unknown"),
    ARCHIVE("archive"),
    AUDIO("audio"),
    BINARY("binary"),
    CODE("code"),
    FONT("font"),
    IMAGE("image"),
    TEXT("text"),
    VIDEO("video"),
    XML("xml");

    override fun toString(): String {
        return value
    }

    companion object {
        fun forName(name: String): FileType {
            val lname = name.trim().lowercase()
            FileType.entries.forEach {
                if (it.value == lname) {
                    return it
                }
            }
            return UNKNOWN
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
        } catch (e: IOException) {
            e.printStackTrace()
        }
    }

    fun getFileType(file: File): FileType {
        when {
            // more specific file types first
            isCodeFile(file) -> {
                return FileType.CODE
            }

            isArchiveFile(file) -> {
                return FileType.ARCHIVE
            }

            isAudioFile(file) -> {
                return FileType.AUDIO
            }

            isFontFile(file) -> {
                return FileType.FONT
            }

            isImageFile(file) -> {
                return FileType.IMAGE
            }

            isVideoFile(file) -> {
                return FileType.VIDEO
            }

            // more general file types last
            isXmlFile(file) -> {
                return FileType.XML
            }

            isTextFile(file) -> {
                return FileType.TEXT
            }

            isBinaryFile(file) -> {
                return FileType.BINARY
            }

            else -> {
                return FileType.UNKNOWN
            }
        }
    }

    fun isArchiveFile(file: File): Boolean {
        return (fileTypeNameMap[FileType.ARCHIVE.value] ?: setOf()).contains(file.name)
                || (fileTypeExtMap[FileType.ARCHIVE.value] ?: setOf()).contains(file.extension.lowercase())
    }

    fun isAudioFile(file: File): Boolean {
        return (fileTypeNameMap[FileType.AUDIO.value] ?: setOf()).contains(file.name)
                || (fileTypeExtMap[FileType.AUDIO.value] ?: setOf()).contains(file.extension.lowercase())
    }

    fun isBinaryFile(file: File): Boolean {
        return (fileTypeNameMap[FileType.BINARY.value] ?: setOf()).contains(file.name)
                || (fileTypeExtMap[FileType.BINARY.value] ?: setOf()).contains(file.extension.lowercase())
    }

    fun isCodeFile(file: File): Boolean {
        return (fileTypeNameMap[FileType.CODE.value] ?: setOf()).contains(file.name)
                || (fileTypeExtMap[FileType.CODE.value] ?: setOf()).contains(file.extension.lowercase())
    }

    fun isFontFile(file: File): Boolean {
        return (fileTypeNameMap[FileType.FONT.value] ?: setOf()).contains(file.name)
                || (fileTypeExtMap[FileType.FONT.value] ?: setOf()).contains(file.extension.lowercase())
    }

    fun isImageFile(file: File): Boolean {
        return (fileTypeNameMap[FileType.IMAGE.value] ?: setOf()).contains(file.name)
                || (fileTypeExtMap[FileType.IMAGE.value] ?: setOf()).contains(file.extension.lowercase())
    }

    fun isTextFile(file: File): Boolean {
        return (fileTypeNameMap[FileType.TEXT.value] ?: setOf()).contains(file.name)
                || (fileTypeExtMap[FileType.TEXT.value] ?: setOf()).contains(file.extension.lowercase())
    }

    fun isUnknownFile(file: File): Boolean {
        return getFileType(file) == FileType.UNKNOWN
    }

    fun isVideoFile(file: File): Boolean {
        return (fileTypeNameMap[FileType.VIDEO.value] ?: setOf()).contains(file.name)
                || (fileTypeExtMap[FileType.VIDEO.value] ?: setOf()).contains(file.extension.lowercase())
    }

    fun isXmlFile(file: File): Boolean {
        return (fileTypeNameMap[FileType.XML.value] ?: setOf()).contains(file.name)
                || (fileTypeExtMap[FileType.XML.value] ?: setOf()).contains(file.extension.lowercase())
    }
}
