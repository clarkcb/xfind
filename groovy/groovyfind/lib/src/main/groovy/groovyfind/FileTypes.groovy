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
        return UNKNOWN
    }
}

class FileTypes {

    private static final String FILE_TYPES_JSON_PATH = '/filetypes.json'
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
            allTextExts.addAll(fileTypeExtMap.get(FileType.CODE.toName()))
            allTextExts.addAll(fileTypeExtMap.get(FileType.TEXT.toName()))
            allTextExts.addAll(fileTypeExtMap.get(FileType.XML.toName()))
            fileTypeExtMap.put(FileType.TEXT.toName(), allTextExts)

            def allTextNames = new HashSet<String>()
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
        return FileType.UNKNOWN
    }

    final boolean isArchiveFile(final Path path) {
        return fileTypeNameMap.get(FileType.ARCHIVE.toName()).contains(path.fileName.toString())
                || fileTypeExtMap.get(FileType.ARCHIVE.toName()).contains(FileUtil.getExtension(path))
    }

    final boolean isAudioFile(final Path path) {
        return fileTypeNameMap.get(FileType.AUDIO.toName()).contains(path.fileName.toString())
                || fileTypeExtMap.get(FileType.AUDIO.toName()).contains(FileUtil.getExtension(path))
    }

    final boolean isBinaryFile(final Path path) {
        return fileTypeNameMap.get(FileType.BINARY.toName()).contains(path.fileName.toString())
                || fileTypeExtMap.get(FileType.BINARY.toName()).contains(FileUtil.getExtension(path))
    }

    final boolean isCodeFile(final Path path) {
        return fileTypeNameMap.get(FileType.CODE.toName()).contains(path.fileName.toString())
                || fileTypeExtMap.get(FileType.CODE.toName()).contains(FileUtil.getExtension(path))
    }

    final boolean isFontFile(final Path path) {
        return fileTypeNameMap.get(FileType.FONT.toName()).contains(path.fileName.toString())
                || fileTypeExtMap.get(FileType.FONT.toName()).contains(FileUtil.getExtension(path))
    }

    final boolean isImageFile(final Path path) {
        return fileTypeNameMap.get(FileType.IMAGE.toName()).contains(path.fileName.toString())
                || fileTypeExtMap.get(FileType.IMAGE.toName()).contains(FileUtil.getExtension(path))
    }

    final boolean isTextFile(final Path path) {
        return fileTypeNameMap.get(FileType.TEXT.toName()).contains(path.fileName.toString())
                || fileTypeExtMap.get(FileType.TEXT.toName()).contains(FileUtil.getExtension(path))
    }

    final boolean isVideoFile(final Path path) {
        return fileTypeNameMap.get(FileType.VIDEO.toName()).contains(path.fileName.toString())
                || fileTypeExtMap.get(FileType.VIDEO.toName()).contains(FileUtil.getExtension(path))
    }

    final boolean isUnknownFile(final Path path) {
        return getFileType(path) == FileType.UNKNOWN
    }

    final boolean isXmlFile(final Path path) {
        return fileTypeNameMap.get(FileType.XML.toName()).contains(path.fileName.toString())
                || fileTypeExtMap.get(FileType.XML.toName()).contains(FileUtil.getExtension(path))
    }

}
