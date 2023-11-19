package groovyfind

import groovy.json.JsonSlurper

import java.nio.file.Path

enum FileType {
    UNKNOWN,
    ARCHIVE,
    AUDIO,
    BINARY,
    CODE,
    FONT,
    IMAGE,
    TEXT,
    VIDEO,
    XML
}

class FileTypes {

    private static final String FILE_TYPES_JSON_PATH = '/filetypes.json'
    private static final String ARCHIVE = 'archive'
    private static final String AUDIO = 'audio'
    private static final String BINARY = 'binary'
    private static final String CODE = 'code'
    private static final String FONT = 'font'
    private static final String IMAGE = 'image'
    private static final String TEXT = 'text'
    // private static final String unknown = "unknown"
    private static final String VIDEO = 'video'
    private static final String XML = 'xml'
    private static final int fileTypeMapCapacity = 8
    private final def fileTypeExtMap = new HashMap<String, Set<String>>(fileTypeMapCapacity)
    private final def fileTypeNameMap = new HashMap<String, Set<String>>(fileTypeMapCapacity)

    private void setFileTypeMapsFromJson() {
        JsonSlurper jsonSlurper = new JsonSlurper()
        def fileTypesInputStream = getClass().getResourceAsStream(FILE_TYPES_JSON_PATH)
        assert fileTypesInputStream != null

        try {
            def jsonObj = jsonSlurper.parse(fileTypesInputStream)
            assert jsonObj instanceof Map
            assert jsonObj.filetypes instanceof List
            def fileTypesList = (List)jsonObj.filetypes

            for (int i=0; i < fileTypesList.size(); i++) {
                def filetypeObj = fileTypesList[i]
                assert filetypeObj instanceof Map
                assert filetypeObj.type instanceof String
                String typeName = filetypeObj.type
                fileTypeExtMap[typeName] = filetypeObj.extensions as Set
                fileTypeNameMap[typeName] = filetypeObj.names as Set
            }

            def allTextExts = new HashSet<String>()
            allTextExts.addAll(fileTypeExtMap.get(CODE))
            allTextExts.addAll(fileTypeExtMap.get(TEXT))
            allTextExts.addAll(fileTypeExtMap.get(XML))
            fileTypeExtMap.put(TEXT, allTextExts)

            def allTextNames = new HashSet<String>()
            allTextNames.addAll(fileTypeNameMap.get(CODE))
            allTextNames.addAll(fileTypeNameMap.get(TEXT))
            allTextNames.addAll(fileTypeNameMap.get(XML))
            fileTypeNameMap.put(TEXT, allTextNames)
        } catch (AssertionError e) {
            e.printStackTrace()
        }
    }

    FileTypes() {
        setFileTypeMapsFromJson()
    }

    static FileType fromName(final String name) {
        switch (name.toLowerCase()) {
            case ARCHIVE:
                return FileType.ARCHIVE
            case AUDIO:
                return FileType.AUDIO
            case BINARY:
                return FileType.BINARY
            case CODE:
                return FileType.CODE
            case FONT:
                return FileType.FONT
            case IMAGE:
                return FileType.IMAGE
            case TEXT:
                return FileType.TEXT
            case VIDEO:
                return FileType.VIDEO
            case XML:
                return FileType.XML
            default:
                return FileType.UNKNOWN
        }
    }

    final FileType getFileType(final Path f) {
        // most specific first
        if (isCodeFile(f)) { return FileType.CODE }
        if (isArchiveFile(f)) { return FileType.ARCHIVE }
        if (isAudioFile(f)) { return FileType.AUDIO }
        if (isFontFile(f)) { return FileType.FONT }
        if (isImageFile(f)) { return FileType.IMAGE }
        if (isVideoFile(f)) { return FileType.VIDEO }
        // most general last
        if (isXmlFile(f)) { return FileType.XML }
        if (isTextFile(f)) { return FileType.TEXT }
        if (isBinaryFile(f)) { return FileType.BINARY }
        return FileType.UNKNOWN
    }

    final boolean isArchiveFile(final Path path) {
        return fileTypeNameMap.get(ARCHIVE).contains(path.fileName.toString())
                || fileTypeExtMap.get(ARCHIVE).contains(FileUtil.getExtension(path))
    }

    final boolean isAudioFile(final Path path) {
        return fileTypeNameMap.get(AUDIO).contains(path.fileName.toString())
                || fileTypeExtMap.get(AUDIO).contains(FileUtil.getExtension(path))
    }

    final boolean isBinaryFile(final Path path) {
        return fileTypeNameMap.get(BINARY).contains(path.fileName.toString())
                || fileTypeExtMap.get(BINARY).contains(FileUtil.getExtension(path))
    }

    final boolean isCodeFile(final Path path) {
        return fileTypeNameMap.get(CODE).contains(path.fileName.toString())
                || fileTypeExtMap.get(CODE).contains(FileUtil.getExtension(path))
    }

    final boolean isFontFile(final Path path) {
        return fileTypeNameMap.get(FONT).contains(path.fileName.toString())
                || fileTypeExtMap.get(FONT).contains(FileUtil.getExtension(path))
    }

    final boolean isImageFile(final Path path) {
        return fileTypeNameMap.get(IMAGE).contains(path.fileName.toString())
                || fileTypeExtMap.get(IMAGE).contains(FileUtil.getExtension(path))
    }

    final boolean isTextFile(final Path path) {
        return fileTypeNameMap.get(TEXT).contains(path.fileName.toString())
                || fileTypeExtMap.get(TEXT).contains(FileUtil.getExtension(path))
    }

    final boolean isVideoFile(final Path path) {
        return fileTypeNameMap.get(VIDEO).contains(path.fileName.toString())
                || fileTypeExtMap.get(VIDEO).contains(FileUtil.getExtension(path))
    }

    final boolean isUnknownFile(final Path path) {
        return getFileType(path) == FileType.UNKNOWN
    }

    final boolean isXmlFile(final Path path) {
        return fileTypeNameMap.get(XML).contains(path.fileName.toString())
                || fileTypeExtMap.get(XML).contains(FileUtil.getExtension(path))
    }

}
