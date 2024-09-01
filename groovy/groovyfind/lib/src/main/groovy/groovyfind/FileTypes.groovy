package groovyfind

import org.sqlite.SQLiteConfig

import java.nio.file.Path
import java.sql.Connection
import java.sql.DriverManager
import java.sql.PreparedStatement
import java.sql.ResultSet
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
    private Map<String, FileType> _extTypeCache
    private Map<String, FileType> _nameTypeCache
    private boolean _nameTypeCacheLoaded = false

    private Connection getConnection() {
        Class.forName("org.sqlite.JDBC")
        if (_conn == null) {
            try {
                def config = new SQLiteConfig()
                config.setReadOnly(true)
                _conn = DriverManager.getConnection("jdbc:sqlite:" + FindConfig.XFIND_DB, config.toProperties())
            } catch (SQLException | ClassNotFoundException e) {
                Logger.logError(e.getMessage())
            }
        }
        return _conn
    }

    FileTypes() {
        _extTypeCache = new HashMap<String, FileType>()
        _nameTypeCache = new HashMap<String, FileType>()
    }

    private Map<String, FileType> getFileTypesForQueryAndParams(final String query, final List<String> params) {
        Connection conn = getConnection()
        Map<String, FileType> results = new HashMap<String, FileType>()
        try {
            PreparedStatement stmt = conn.prepareStatement(query)
            for (int i = 0; i < params.size(); i++) {
                stmt.setString(i + 1, params.get(i))
            }
            ResultSet rs = stmt.executeQuery()
            while (rs.next()) {
                String key = rs.getString(1)
                int fileTypeId = rs.getInt(2) - 1
                results.put(key, fileTypes.get(fileTypeId))
            }
        } catch (SQLException e) {
            Logger.logError(e.getMessage())
        }
        return results
    }

    private void loadNameTypeCache() {
        String query = "SELECT name, file_type_id FROM file_name"
        _nameTypeCache = getFileTypesForQueryAndParams(query, [])
        _nameTypeCacheLoaded = true
    }

    private FileType getFileTypeForQueryAndParams(final String query, final List<String> params) {
        Connection conn = getConnection()
        try {
            PreparedStatement stmt = conn.prepareStatement(query)
            for (int i = 0; i < params.size(); i++) {
                stmt.setString(i + 1, params.get(i))
            }
            ResultSet rs = stmt.executeQuery()
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
        if (!_nameTypeCacheLoaded) {
            loadNameTypeCache()
        }
        if (_nameTypeCache.containsKey(fileName)) {
            return _nameTypeCache.get(fileName)
        }
//        String query = "SELECT file_type_id FROM file_name WHERE name = ?"
//        return getFileTypeForQueryAndParams(query, [fileName])
        return FileType.UNKNOWN
    }

    final FileType getFileTypeForExtension(final String extension) {
        if (extension == null || extension.isEmpty()) {
            return FileType.UNKNOWN
        }
        if (_extTypeCache.containsKey(extension)) {
            return _extTypeCache.get(extension)
        }
        String query = "SELECT file_type_id FROM file_extension WHERE extension = ?"
        FileType fileType = getFileTypeForQueryAndParams(query, [extension])
        _extTypeCache.put(extension, fileType)
        return fileType
    }

    final FileType getFileType(final Path f) {
        FileType fileType = getFileTypeForFileName(f.getFileName().toString())
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
        FileType fileType = getFileType(path)
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
