package groovyfind

import org.sqlite.SQLiteConfig
import org.sqlite.JDBC

import java.nio.file.Path
import java.sql.Connection
import java.sql.DriverManager
import java.sql.SQLException

enum FileType {
    UNKNOWN('unknown'),
    ARCHIVE('archive'),
    AUDIO('audio'),
    BINARY('binary'),
    CODE('code'),
    FONT('font'),
    IMAGE('image'),
    TEXT('text'),
    VIDEO('video'),
    XML('xml');

    private final String name

    FileType(final String name) {
        this.name = name
    }

    String toName() {
        return name
    }

    static FileType forName(final String name) {
        String lname = name.trim().toLowerCase()
        for (FileType fileType : values()) {
            if (fileType.name == lname) {
                return fileType
            }
        }
        UNKNOWN
    }
}

class FileTypes {
    private static final List<FileType> fileTypes = List.of(
            FileType.UNKNOWN,
            FileType.ARCHIVE,
            FileType.AUDIO,
            FileType.BINARY,
            FileType.CODE,
            FileType.FONT,
            FileType.IMAGE,
            FileType.TEXT,
            FileType.VIDEO,
            FileType.XML
    )
    private Connection _conn = null
    private HashMap<String, FileType> _extFileTypeCache

    private Connection getConnection() {
        Class.forName("org.sqlite.JDBC")
        if (_conn == null) {
            try {
                def config = new SQLiteConfig()
                config.setReadOnly(true)
                _conn = DriverManager.getConnection("jdbc:sqlite:" + FindConfig.XFINDDB, config.toProperties())
            } catch (SQLException | ClassNotFoundException e) {
                Logger.logError(e.getMessage())
            }
        }
        return _conn
    }

    FileTypes() {
        _extFileTypeCache = new HashMap<String, FileType>()
    }

    private FileType getFileTypeForQueryAndElem(final String query, final String elem) {
        Connection conn = getConnection()
        try {
            def stmt = conn.prepareStatement(query)
            stmt.setString(1, elem)
            def rs = stmt.executeQuery()
            if (rs.next()) {
                int fileTypeId = rs.getInt("file_type_id") - 1
                return fileTypes.get(fileTypeId)
            }
        } catch (SQLException e) {
            Logger.logError(e.getMessage())
        }
        return FileType.UNKNOWN
    }

    final FileType getFileTypeForFileName(final String fileName) {
        def query = "SELECT file_type_id FROM file_name WHERE name = ?"
        return getFileTypeForQueryAndElem(query, fileName)
    }

    final FileType getFileTypeForExtension(final String extension) {
        if (_extFileTypeCache.containsKey(extension)) {
            return _extFileTypeCache.get(extension)
        }
        def query = "SELECT file_type_id FROM file_extension WHERE extension = ?"
        FileType fileType = getFileTypeForQueryAndElem(query, extension)
        _extFileTypeCache.put(extension, fileType)
        return fileType
    }

    final FileType getFileType(final Path f) {
        def fileType = getFileTypeForFileName(f.getFileName().toString())
        if (fileType != FileType.UNKNOWN) {
            return fileType
        }
        return getFileTypeForExtension(FileUtil.getExtension(f))
    }

    final boolean isArchiveFile(final Path path) {
        return getFileType(path) == FileType.ARCHIVE
    }

    final boolean isAudioFile(final Path path) {
        return getFileType(path) == FileType.AUDIO
    }

    final boolean isBinaryFile(final Path path) {
        return getFileType(path) == FileType.BINARY
    }

    final boolean isCodeFile(final Path path) {
        return getFileType(path) == FileType.CODE
    }

    final boolean isFontFile(final Path path) {
        return getFileType(path) == FileType.FONT
    }

    final boolean isImageFile(final Path path) {
        return getFileType(path) == FileType.IMAGE
    }

    final boolean isTextFile(final Path path) {
        def fileType = getFileType(path)
        return fileType == FileType.TEXT
                || fileType == FileType.CODE
                || fileType == FileType.XML
    }

    final boolean isVideoFile(final Path path) {
        return getFileType(path) == FileType.VIDEO
    }

    final boolean isUnknownFile(final Path path) {
        getFileType(path) == FileType.UNKNOWN
    }

    final boolean isXmlFile(final Path path) {
        return getFileType(path) == FileType.XML
    }
}
