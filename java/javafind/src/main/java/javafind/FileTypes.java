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
    private static final String archive = "archive";
    private static final String binary = "binary";
    private static final String code = "code";
    private static final String text = "text";
    // private static final String unknown = "unknown";
    private static final String xml = "xml";
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
            allTextExts.addAll(fileTypeExtMap.get(code));
            allTextExts.addAll(fileTypeExtMap.get(text));
            allTextExts.addAll(fileTypeExtMap.get(xml));
            fileTypeExtMap.put(text, allTextExts);

            var allTextNames = new HashSet<String>();
            allTextNames.addAll(fileTypeNameMap.get(code));
            allTextNames.addAll(fileTypeNameMap.get(text));
            allTextNames.addAll(fileTypeNameMap.get(xml));
            fileTypeNameMap.put(text, allTextNames);
        } catch (AssertionError e) {
            e.printStackTrace();
        }
    }

    public FileTypes() {
        setFileTypeMapsFromJson();
    }

    static FileType fromName(final String name) {
        var lname = name.toLowerCase();
        if (lname.equals(code)) return FileType.CODE;
        if (lname.equals(xml)) return FileType.XML;
        if (lname.equals(text)) return FileType.TEXT;
        if (lname.equals(binary)) return FileType.BINARY;
        if (lname.equals(archive)) return FileType.ARCHIVE;
        return FileType.UNKNOWN;
    }

    final FileType getFileType(final Path f) {
        if (isCodeFile(f)) return FileType.CODE;
        if (isXmlFile(f)) return FileType.XML;
        if (isTextFile(f)) return FileType.TEXT;
        if (isBinaryFile(f)) return FileType.BINARY;
        if (isArchiveFile(f)) return FileType.ARCHIVE;
        return FileType.UNKNOWN;
    }

    final boolean isArchiveFile(final Path path) {
        return fileTypeNameMap.get(archive).contains(path.getFileName().toString())
            || fileTypeExtMap.get(archive).contains(FileUtil.getExtension(path));
    }

    final boolean isBinaryFile(final Path path) {
        return fileTypeNameMap.get(binary).contains(path.getFileName().toString())
            || fileTypeExtMap.get(binary).contains(FileUtil.getExtension(path));
    }

    public final boolean isCodeFile(final Path path) {
        return fileTypeNameMap.get(code).contains(path.getFileName().toString())
            || fileTypeExtMap.get(code).contains(FileUtil.getExtension(path));
    }

    final boolean isTextFile(final Path path) {
        return fileTypeNameMap.get(text).contains(path.getFileName().toString())
            || fileTypeExtMap.get(text).contains(FileUtil.getExtension(path));
    }

    final boolean isUnknownFile(final Path path) {
        return getFileType(path) == FileType.UNKNOWN;
    }

    public final boolean isXmlFile(final Path path) {
        return fileTypeNameMap.get(xml).contains(path.getFileName().toString())
            || fileTypeExtMap.get(xml).contains(FileUtil.getExtension(path));
    }
}
