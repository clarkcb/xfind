package ktfind

import java.io.IOException

import org.json.JSONObject
import org.json.JSONTokener
import java.nio.file.Path
import kotlin.io.path.extension

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

    fun getFileType(path: Path): FileType {
        when {
            // more specific file types first
            isCodeFile(path) -> {
                return FileType.CODE
            }

            isArchiveFile(path) -> {
                return FileType.ARCHIVE
            }

            isAudioFile(path) -> {
                return FileType.AUDIO
            }

            isFontFile(path) -> {
                return FileType.FONT
            }

            isImageFile(path) -> {
                return FileType.IMAGE
            }

            isVideoFile(path) -> {
                return FileType.VIDEO
            }

            // more general file types last
            isXmlFile(path) -> {
                return FileType.XML
            }

            isTextFile(path) -> {
                return FileType.TEXT
            }

            isBinaryFile(path) -> {
                return FileType.BINARY
            }

            else -> {
                return FileType.UNKNOWN
            }
        }
    }

    fun isArchiveFile(path: Path): Boolean {
        return (fileTypeNameMap[FileType.ARCHIVE.value] ?: setOf()).contains(path.fileName.toString())
                || (fileTypeExtMap[FileType.ARCHIVE.value] ?: setOf()).contains(path.extension.lowercase())
    }

    fun isAudioFile(path: Path): Boolean {
        return (fileTypeNameMap[FileType.AUDIO.value] ?: setOf()).contains(path.fileName.toString())
                || (fileTypeExtMap[FileType.AUDIO.value] ?: setOf()).contains(path.extension.lowercase())
    }

    fun isBinaryFile(path: Path): Boolean {
        return (fileTypeNameMap[FileType.BINARY.value] ?: setOf()).contains(path.fileName.toString())
                || (fileTypeExtMap[FileType.BINARY.value] ?: setOf()).contains(path.extension.lowercase())
    }

    fun isCodeFile(path: Path): Boolean {
        return (fileTypeNameMap[FileType.CODE.value] ?: setOf()).contains(path.fileName.toString())
                || (fileTypeExtMap[FileType.CODE.value] ?: setOf()).contains(path.extension.lowercase())
    }

    fun isFontFile(path: Path): Boolean {
        return (fileTypeNameMap[FileType.FONT.value] ?: setOf()).contains(path.fileName.toString())
                || (fileTypeExtMap[FileType.FONT.value] ?: setOf()).contains(path.extension.lowercase())
    }

    fun isImageFile(path: Path): Boolean {
        return (fileTypeNameMap[FileType.IMAGE.value] ?: setOf()).contains(path.fileName.toString())
                || (fileTypeExtMap[FileType.IMAGE.value] ?: setOf()).contains(path.extension.lowercase())
    }

    fun isTextFile(path: Path): Boolean {
        return (fileTypeNameMap[FileType.TEXT.value] ?: setOf()).contains(path.fileName.toString())
                || (fileTypeExtMap[FileType.TEXT.value] ?: setOf()).contains(path.extension.lowercase())
    }

    fun isUnknownFile(path: Path): Boolean {
        return getFileType(path) == FileType.UNKNOWN
    }

    fun isVideoFile(path: Path): Boolean {
        return (fileTypeNameMap[FileType.VIDEO.value] ?: setOf()).contains(path.fileName.toString())
                || (fileTypeExtMap[FileType.VIDEO.value] ?: setOf()).contains(path.extension.lowercase())
    }

    fun isXmlFile(path: Path): Boolean {
        return (fileTypeNameMap[FileType.XML.value] ?: setOf()).contains(path.fileName.toString())
                || (fileTypeExtMap[FileType.XML.value] ?: setOf()).contains(path.extension.lowercase())
    }
}
