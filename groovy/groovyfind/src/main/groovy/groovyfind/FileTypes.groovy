package groovyfind

import groovy.json.JsonSlurper

import java.nio.file.Path

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
    private static final String UNKNOWN = 'unknown'
    private static final String FILE_TYPES_JSON_PATH = '/filetypes.json'
    private static final int fileTypeMapCapacity = 8
    private final Map<String, Set<String>> fileTypeExtMap = new HashMap<String, Set<String>>(fileTypeMapCapacity)
    private final Map<String, Set<String>> fileTypeNameMap = new HashMap<String, Set<String>>(fileTypeMapCapacity)

    private void setFileTypeMapsFromJson() {
        JsonSlurper jsonSlurper = new JsonSlurper()
        InputStream fileTypesInputStream = getClass().getResourceAsStream(FILE_TYPES_JSON_PATH)
        assert fileTypesInputStream != null

        try {
            def jsonObj = jsonSlurper.parse(fileTypesInputStream)
            assert jsonObj instanceof Map
            assert jsonObj.filetypes instanceof List
            List fileTypesList = (List)jsonObj.filetypes

            for (int i=0; i < fileTypesList.size(); i++) {
                def filetypeObj = fileTypesList[i]
                assert filetypeObj instanceof Map
                assert filetypeObj.type instanceof String
                String typeName = filetypeObj.type
                fileTypeExtMap[typeName] = filetypeObj.extensions as Set
                fileTypeNameMap[typeName] = filetypeObj.names as Set
            }

            Set<String> allTextExts = new HashSet<String>()
            allTextExts.addAll(fileTypeExtMap.get(FileType.CODE.toName()))
            allTextExts.addAll(fileTypeExtMap.get(FileType.TEXT.toName()))
            allTextExts.addAll(fileTypeExtMap.get(FileType.XML.toName()))
            fileTypeExtMap.put(FileType.TEXT.toName(), allTextExts)

            Set<String> allTextNames = new HashSet<String>()
            allTextNames.addAll(fileTypeNameMap.get(FileType.CODE.toName()))
            allTextNames.addAll(fileTypeNameMap.get(FileType.TEXT.toName()))
            allTextNames.addAll(fileTypeNameMap.get(FileType.XML.toName()))
            fileTypeNameMap.put(FileType.TEXT.toName(), allTextNames)
        } catch (AssertionError e) {
            e.printStackTrace()
        }
    }

    FileTypes() {
        setFileTypeMapsFromJson()
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
        FileType.UNKNOWN
    }

    final boolean isArchiveFile(final Path path) {
        path.fileName.toString() in fileTypeNameMap.get(FileType.ARCHIVE.toName())
                || FileUtil.getExtension(path) in fileTypeExtMap.get(FileType.ARCHIVE.toName())
    }

    final boolean isAudioFile(final Path path) {
        path.fileName.toString() in fileTypeNameMap.get(FileType.AUDIO.toName())
                || FileUtil.getExtension(path) in fileTypeExtMap.get(FileType.AUDIO.toName())
    }

    final boolean isBinaryFile(final Path path) {
        path.fileName.toString() in fileTypeNameMap.get(FileType.BINARY.toName())
                || FileUtil.getExtension(path) in fileTypeExtMap.get(FileType.BINARY.toName())
    }

    final boolean isCodeFile(final Path path) {
        path.fileName.toString() in fileTypeNameMap.get(FileType.CODE.toName())
                || FileUtil.getExtension(path) in fileTypeExtMap.get(FileType.CODE.toName())
    }

    final boolean isFontFile(final Path path) {
        path.fileName.toString() in fileTypeNameMap.get(FileType.FONT.toName())
                || FileUtil.getExtension(path) in fileTypeExtMap.get(FileType.FONT.toName())
    }

    final boolean isImageFile(final Path path) {
        path.fileName.toString() in fileTypeNameMap.get(FileType.IMAGE.toName())
                || FileUtil.getExtension(path) in fileTypeExtMap.get(FileType.IMAGE.toName())
    }

    final boolean isTextFile(final Path path) {
        path.fileName.toString() in fileTypeNameMap.get(FileType.TEXT.toName())
                || FileUtil.getExtension(path) in fileTypeExtMap.get(FileType.TEXT.toName())
    }

    final boolean isVideoFile(final Path path) {
        path.fileName.toString() in fileTypeNameMap.get(FileType.VIDEO.toName())
                || FileUtil.getExtension(path) in fileTypeExtMap.get(FileType.VIDEO.toName())
    }

    final boolean isUnknownFile(final Path path) {
        getFileType(path) == FileType.UNKNOWN
    }

    final boolean isXmlFile(final Path path) {
        path.fileName.toString() in fileTypeNameMap.get(FileType.XML.toName())
                || FileUtil.getExtension(path) in fileTypeExtMap.get(FileType.XML.toName())
    }
}
