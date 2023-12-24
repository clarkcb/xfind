package javafind;

import org.json.JSONObject;
import org.json.JSONTokener;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class FileTypes {
    private static final String FILE_TYPES_JSON_PATH = "/filetypes.json";
    private static final int fileTypeMapCapacity = 8;
    private final Map<String, Set<String>> fileTypeExtMap = new HashMap<>(fileTypeMapCapacity);
    private final Map<String, Set<String>> fileTypeNameMap = new HashMap<>(fileTypeMapCapacity);

    private void setFileTypeMapsFromJson() {
        var fileTypesInputStream = getClass().getResourceAsStream(FILE_TYPES_JSON_PATH);

        try {
            assert fileTypesInputStream != null;
            var jsonObj = new JSONObject(new JSONTokener(fileTypesInputStream));
            var filetypesArray = jsonObj.getJSONArray("filetypes");

            for (int i=0; i < filetypesArray.length(); i++) {
                var filetypeObj = filetypesArray.getJSONObject(i);
                var typeName = filetypeObj.getString("type");
                fileTypeExtMap.put(typeName, filetypeObj.getJSONArray("extensions").toList().stream()
                        .map(Object::toString).collect(Collectors.toCollection(HashSet::new)));
                fileTypeNameMap.put(typeName, filetypeObj.getJSONArray("names").toList().stream()
                        .map(Object::toString).collect(Collectors.toCollection(HashSet::new)));
            }

            var allTextExts = new HashSet<String>();
            allTextExts.addAll(fileTypeExtMap.get(FileType.CODE.toName()));
            allTextExts.addAll(fileTypeExtMap.get(FileType.TEXT.toName()));
            allTextExts.addAll(fileTypeExtMap.get(FileType.XML.toName()));
            fileTypeExtMap.put(FileType.TEXT.toName(), allTextExts);

            var allTextNames = new HashSet<String>();
            allTextNames.addAll(fileTypeNameMap.get(FileType.CODE.toName()));
            allTextNames.addAll(fileTypeNameMap.get(FileType.TEXT.toName()));
            allTextNames.addAll(fileTypeNameMap.get(FileType.XML.toName()));
            fileTypeNameMap.put(FileType.TEXT.toName(), allTextNames);
        } catch (AssertionError e) {
            e.printStackTrace();
        }
    }

    public FileTypes() {
        setFileTypeMapsFromJson();
    }

    final FileType getFileType(final Path f) {
        // most specific first
        if (isCodeFile(f)) return FileType.CODE;
        if (isArchiveFile(f)) return FileType.ARCHIVE;
        if (isAudioFile(f)) return FileType.AUDIO;
        if (isFontFile(f)) return FileType.FONT;
        if (isImageFile(f)) return FileType.IMAGE;
        if (isVideoFile(f)) return FileType.VIDEO;
        // most general last
        if (isXmlFile(f)) return FileType.XML;
        if (isTextFile(f)) return FileType.TEXT;
        if (isBinaryFile(f)) return FileType.BINARY;
        return FileType.UNKNOWN;
    }

    final boolean isArchiveFile(final Path path) {
        return fileTypeNameMap.get(FileType.ARCHIVE.toName()).contains(path.getFileName().toString())
            || fileTypeExtMap.get(FileType.ARCHIVE.toName()).contains(FileUtil.getExtension(path));
    }

    final boolean isAudioFile(final Path path) {
        return fileTypeNameMap.get(FileType.AUDIO.toName()).contains(path.getFileName().toString())
            || fileTypeExtMap.get(FileType.AUDIO.toName()).contains(FileUtil.getExtension(path));
    }

    final boolean isBinaryFile(final Path path) {
        return fileTypeNameMap.get(FileType.BINARY.toName()).contains(path.getFileName().toString())
            || fileTypeExtMap.get(FileType.BINARY.toName()).contains(FileUtil.getExtension(path));
    }

    public final boolean isCodeFile(final Path path) {
        return fileTypeNameMap.get(FileType.CODE.toName()).contains(path.getFileName().toString())
            || fileTypeExtMap.get(FileType.CODE.toName()).contains(FileUtil.getExtension(path));
    }

    public final boolean isFontFile(final Path path) {
        return fileTypeNameMap.get(FileType.FONT.toName()).contains(path.getFileName().toString())
            || fileTypeExtMap.get(FileType.FONT.toName()).contains(FileUtil.getExtension(path));
    }

    public final boolean isImageFile(final Path path) {
        return fileTypeNameMap.get(FileType.IMAGE.toName()).contains(path.getFileName().toString())
            || fileTypeExtMap.get(FileType.IMAGE.toName()).contains(FileUtil.getExtension(path));
    }

    final boolean isTextFile(final Path path) {
        return fileTypeNameMap.get(FileType.TEXT.toName()).contains(path.getFileName().toString())
            || fileTypeExtMap.get(FileType.TEXT.toName()).contains(FileUtil.getExtension(path));
    }

    final boolean isVideoFile(final Path path) {
        return fileTypeNameMap.get(FileType.VIDEO.toName()).contains(path.getFileName().toString())
            || fileTypeExtMap.get(FileType.VIDEO.toName()).contains(FileUtil.getExtension(path));
    }

    final boolean isUnknownFile(final Path path) {
        return getFileType(path) == FileType.UNKNOWN;
    }

    public final boolean isXmlFile(final Path path) {
        return fileTypeNameMap.get(FileType.XML.toName()).contains(path.getFileName().toString())
            || fileTypeExtMap.get(FileType.XML.toName()).contains(FileUtil.getExtension(path));
    }
}
