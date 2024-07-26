package ktfind

import org.sqlite.SQLiteConfig
import java.nio.file.Path
import java.sql.*
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

    private val SELECT_FILE_TYPE_ID_FOR_FILE_NAME: String = "SELECT file_type_id FROM file_name WHERE name = ?"
    private val SELECT_FILE_TYPE_ID_FOR_EXTENSION: String = "SELECT file_type_id FROM file_extension WHERE extension = ?"

    private var _conn: Connection? = null
    private val _fileTypeCache = HashMap<String, FileType>()

    private fun getConnection(): Connection? {
        if (_conn == null) {
            try {
                val cls = Class.forName("org.sqlite.JDBC")
                val config = SQLiteConfig()
                config.setReadOnly(true)
                _conn = DriverManager.getConnection("jdbc:sqlite:" + FindConfig.XFINDDB, config.toProperties())
            } catch (e: Exception) {
                logError(e.message!!)
            }
        }
        return _conn
    }

    private fun getFileTypeForQueryAndElem(query: String, elem: String): FileType {
        val conn = getConnection()
        try {
            val stmt = conn!!.prepareStatement(query)
            stmt.setString(1, elem)
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
        return getFileTypeForQueryAndElem(SELECT_FILE_TYPE_ID_FOR_FILE_NAME, fileName)
    }

    private fun getFileTypeForExtension(fileExt: String): FileType {
        if (_fileTypeCache.containsKey(fileExt)) {
            return _fileTypeCache[fileExt]!!
        }
        val fileType = getFileTypeForQueryAndElem(SELECT_FILE_TYPE_ID_FOR_EXTENSION, fileExt)
        _fileTypeCache[fileExt] = fileType
        return fileType
    }

    fun getFileType(f: Path): FileType {
        val fileType = getFileTypeForFileName(f.fileName.toString())
        if (fileType !== FileType.UNKNOWN) {
            return fileType
        }
        return getFileTypeForExtension(f.extension)
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
