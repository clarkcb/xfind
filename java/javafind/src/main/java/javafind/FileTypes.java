package javafind;

import org.sqlite.SQLiteConfig;

import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class FileTypes {
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
    );
    private static final String SELECT_FILE_NAME_ENTRIES = "SELECT name, file_type_id FROM file_name";
    // private static final String SELECT_FILE_TYPE_ID_FOR_FILE_NAME = "SELECT file_type_id FROM file_name WHERE name = ?";
    private static final String SELECT_FILE_TYPE_ID_FOR_EXTENSION = "SELECT file_type_id FROM file_extension WHERE extension = ?";

    private Connection _conn = null;
    private final Map<String, FileType> _extTypeCache;
    private final Map<String, FileType> _nameTypeCache;
    private boolean _nameTypeCacheLoaded = false;

    private Connection getConnection() {
        if (_conn == null) {
            try {
                // This first line is supposedly needed to load the correct driver
                var cls = Class.forName("org.sqlite.JDBC");
                SQLiteConfig config = new SQLiteConfig();
                config.setReadOnly(true);
                _conn = DriverManager.getConnection("jdbc:sqlite:" + FindConfig.XFIND_DB, config.toProperties());
            } catch (SQLException | ClassNotFoundException e) {
                Logger.logError(e.getMessage());
            }
        }
        return _conn;
    }

    public FileTypes() {
        _extTypeCache = new HashMap<>();
        _nameTypeCache = new HashMap<>();
    }

    private Map<String, FileType> getFileTypesForQueryAndParams(final String query, final List<String> params) {
        var conn = getConnection();
        var results = new HashMap<String, FileType>();
        try {
            var stmt = conn.prepareStatement(query);
            for (int i = 0; i < params.size(); i++) {
                stmt.setString(i + 1, params.get(i));
            }
            var rs = stmt.executeQuery();
            while (rs.next()) {
                var key = rs.getString(1);
                int fileTypeId = rs.getInt(2) - 1;
                results.put(key, fileTypes.get(fileTypeId));
            }
        } catch (SQLException e) {
            Logger.logError(e.getMessage());
        }
        return results;
    }

    private void loadNameTypeCache() {
        _nameTypeCache.putAll(getFileTypesForQueryAndParams(SELECT_FILE_NAME_ENTRIES, List.of()));
        _nameTypeCacheLoaded = true;
    }

    private FileType getFileTypeForQueryAndParams(final String query, final List<String> params) {
        var conn = getConnection();
        try {
            var stmt = conn.prepareStatement(query);
            for (int i = 0; i < params.size(); i++) {
                stmt.setString(i + 1, params.get(i));
            }
            var rs = stmt.executeQuery();
            if (rs.next()) {
                int fileTypeId = rs.getInt("file_type_id") - 1;
                return fileTypes.get(fileTypeId);
            }
        } catch (SQLException e) {
            Logger.logError(e.getMessage());
        }
        return FileType.UNKNOWN;
    }

    private FileType getFileTypeForFileName(final String fileName) {
        if (!_nameTypeCacheLoaded) {
            loadNameTypeCache();
        }
        if (_nameTypeCache.containsKey(fileName)) {
            return _nameTypeCache.get(fileName);
        }
//        return getFileTypeForQueryAndParams(SELECT_FILE_TYPE_ID_FOR_FILE_NAME, List.of(fileName));
        return FileType.UNKNOWN;
    }

    private FileType getFileTypeForExtension(final String fileExt) {
        if (fileExt == null || fileExt.isEmpty()) {
            return FileType.UNKNOWN;
        }
        if (_extTypeCache.containsKey(fileExt)) {
            return _extTypeCache.get(fileExt);
        }
        var fileType = getFileTypeForQueryAndParams(SELECT_FILE_TYPE_ID_FOR_EXTENSION, List.of(fileExt));
        _extTypeCache.put(fileExt, fileType);
        return fileType;
    }

    final FileType getFileType(final Path f) {
        var fileType = getFileTypeForFileName(f.getFileName().toString());
        if (fileType != FileType.UNKNOWN) {
            return fileType;
        }
        return getFileTypeForExtension(FileUtil.getExtension(f));
    }

    final boolean isArchiveFile(final Path path) {
        return getFileType(path) == FileType.ARCHIVE;
    }

    final boolean isAudioFile(final Path path) {
        return getFileType(path) == FileType.AUDIO;
    }

    final boolean isBinaryFile(final Path path) {
        return getFileType(path) == FileType.BINARY;
    }

    public final boolean isCodeFile(final Path path) {
        return getFileType(path) == FileType.CODE;
    }

    public final boolean isFontFile(final Path path) {
        return getFileType(path) == FileType.FONT;
    }

    public final boolean isImageFile(final Path path) {
        return getFileType(path) == FileType.IMAGE;
    }

    final boolean isTextFile(final Path path) {
        var fileType = getFileType(path);
        return fileType == FileType.TEXT
                || fileType == FileType.CODE
                || fileType == FileType.XML;
    }

    final boolean isVideoFile(final Path path) {
        return getFileType(path) == FileType.VIDEO;
    }

    final boolean isUnknownFile(final Path path) {
        return getFileType(path) == FileType.UNKNOWN;
    }

    public final boolean isXmlFile(final Path path) {
        return getFileType(path) == FileType.XML;
    }
}
