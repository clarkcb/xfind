package ktfind

import org.sqlite.SQLiteConfig
import java.nio.file.Path
import java.sql.Connection
import java.sql.DriverManager
import java.sql.SQLException

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

class FileTypes {
    private val fileTypes = listOf(
        FileType.UNKNOWN,
        FileType.ARCHIVE,
        FileType.AUDIO,
        FileType.BINARY,
        FileType.CODE,
        FileType.FONT,
        FileType.IMAGE,
        FileType.TEXT,
        FileType.VIDEO,
        FileType.XML)

    private val SELECT_FILE_NAME_ENTRIES: String = "SELECT name, file_type_id FROM file_name"
    private val SELECT_FILE_TYPE_ID_FOR_FILE_NAME: String = "SELECT file_type_id FROM file_name WHERE name = ?"
    private val SELECT_FILE_TYPE_ID_FOR_EXTENSION: String = "SELECT file_type_id FROM file_extension WHERE extension = ?"

    private var _conn: Connection? = null
    private val _extTypeCache = HashMap<String, FileType>()
    private val _nameTypeCache = HashMap<String, FileType>()
    private var _nameTypeCachedLoaded = false

    private fun getConnection(): Connection? {
        if (_conn == null) {
            try {
                val cls = Class.forName("org.sqlite.JDBC")
                val config = SQLiteConfig()
                config.setReadOnly(true)
                _conn = DriverManager.getConnection("jdbc:sqlite:" + FindConfig.XFIND_DB, config.toProperties())
            } catch (e: Exception) {
                logError(e.message!!)
            }
        }
        return _conn
    }

    private fun getFileTypesForQueryAndParams(query: String, params: List<String>): Map<String, FileType> {
        val conn = getConnection()
        val results = HashMap<String, FileType>()
        try {
            val stmt = conn!!.prepareStatement(query)
            for (i in params.indices) {
                stmt.setString(i + 1, params[i])
            }
            val rs = stmt.executeQuery()
            while (rs.next()) {
                val key = rs.getString(1)
                val fileTypeId = rs.getInt(2) - 1
                results[key] = fileTypes[fileTypeId]
            }
        } catch (e: SQLException) {
            logError(e.message!!)
        }
        return results
    }

    private fun loadNameTypeCache() {
        val results = getFileTypesForQueryAndParams(SELECT_FILE_NAME_ENTRIES, emptyList())
        _nameTypeCache.putAll(results)
        _nameTypeCachedLoaded = true
    }

    private fun getFileTypeForQueryAndParams(query: String, params: List<String>): FileType {
        val conn = getConnection()
        try {
            val stmt = conn!!.prepareStatement(query)
            for (i in params.indices) {
                stmt.setString(i + 1, params[i])
            }
            val rs = stmt.executeQuery()
            if (rs.next()) {
                val fileTypeId = rs.getInt("file_type_id") - 1
                return fileTypes[fileTypeId]
            }
        } catch (e: SQLException) {
            logError(e.message!!)
        }
        return FileType.UNKNOWN
    }

    private fun getFileTypeForFileName(fileName: String): FileType {
        if (!_nameTypeCachedLoaded) {
            loadNameTypeCache()
        }
        if (_nameTypeCache.containsKey(fileName)) {
            return _nameTypeCache[fileName]!!
        }
//        return getFileTypeForQueryAndParams(SELECT_FILE_TYPE_ID_FOR_FILE_NAME, listOf(fileName))
        return FileType.UNKNOWN
    }

    private fun getFileTypeForExtension(fileExt: String): FileType {
        if (fileExt.isEmpty()) {
            return FileType.UNKNOWN
        }
        if (_extTypeCache.containsKey(fileExt)) {
            return _extTypeCache[fileExt]!!
        }
        val fileType = getFileTypeForQueryAndParams(SELECT_FILE_TYPE_ID_FOR_EXTENSION, listOf(fileExt))
        _extTypeCache[fileExt] = fileType
        return fileType
    }

    fun getFileType(path: Path): FileType {
        val fileType = getFileTypeForFileName(path.fileName.toString())
        if (fileType !== FileType.UNKNOWN) {
            return fileType
        }
        return getFileTypeForExtension(FileUtil.getExtension(path))
    }

    fun isArchiveFile(path: Path): Boolean {
        return getFileType(path) == FileType.ARCHIVE
    }

    fun isAudioFile(path: Path): Boolean {
        return getFileType(path) == FileType.AUDIO
    }

    fun isBinaryFile(path: Path): Boolean {
        return getFileType(path) == FileType.BINARY
    }

    fun isCodeFile(path: Path): Boolean {
        return getFileType(path) == FileType.CODE
    }

    fun isFontFile(path: Path): Boolean {
        return getFileType(path) == FileType.FONT
    }

    fun isImageFile(path: Path): Boolean {
        return getFileType(path) == FileType.IMAGE
    }

    fun isTextFile(path: Path): Boolean {
        val fileType = getFileType(path)
        return fileType == FileType.TEXT
                || fileType == FileType.CODE
                || fileType == FileType.XML
    }

    fun isUnknownFile(path: Path): Boolean {
        return getFileType(path) == FileType.UNKNOWN
    }

    fun isVideoFile(path: Path): Boolean {
        return getFileType(path) == FileType.VIDEO
    }

    fun isXmlFile(path: Path): Boolean {
        return getFileType(path) == FileType.XML
    }
}
