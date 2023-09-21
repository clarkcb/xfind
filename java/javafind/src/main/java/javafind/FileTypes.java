package javafind;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class FileTypes {
    private static final String FILETYPESJSONPATH = "/filetypes.json";
    private static final String archive = "archive";
    private static final String binary = "binary";
    private static final String code = "code";
    private static final String text = "text";
    private static final String unknown = "unknown";
    private static final String xml = "xml";
    private static final int fileTypeMapCapacity = 8;
    private final Map<String, Set<String>> fileTypeExtMap = new HashMap<>(fileTypeMapCapacity);
    private final Map<String, Set<String>> fileTypeNameMap = new HashMap<>(fileTypeMapCapacity);

    private void setFileTypeMapsFromJson() {
        // Map<String, Set<String>> fileTypeExtMap = new HashMap<>(fileTypeKeys);
        InputStream fileTypesInputStream = getClass().getResourceAsStream(FILETYPESJSONPATH);

        try {
            assert fileTypesInputStream != null;
            var obj = new JSONParser().parse(new InputStreamReader(fileTypesInputStream));
            var jsonObj = (JSONObject)obj;
            var filetypesArray = (JSONArray) jsonObj.get("filetypes");

            for (Object o : filetypesArray) {
                Map<String, Object> filetypeMap = (Map<String, Object>) o;
                var typeName = (String) filetypeMap.get("type");
                var extArray = (JSONArray) filetypeMap.get("extensions");
                Set<String> extSet = new HashSet<String>(extArray);
                fileTypeExtMap.put(typeName, extSet);
                var nameArray = (JSONArray) filetypeMap.get("names");
                Set<String> nameSet = new HashSet<String>(nameArray);
                fileTypeNameMap.put(typeName, nameSet);
            }

            Set<String> allTextExts = new HashSet<>();
            allTextExts.addAll(fileTypeExtMap.get(code));
            allTextExts.addAll(fileTypeExtMap.get(text));
            allTextExts.addAll(fileTypeExtMap.get(xml));
            fileTypeExtMap.put(text, allTextExts);

            Set<String> allTextNames = new HashSet<>();
            allTextNames.addAll(fileTypeNameMap.get(code));
            allTextNames.addAll(fileTypeNameMap.get(text));
            allTextNames.addAll(fileTypeNameMap.get(xml));
            fileTypeNameMap.put(text, allTextNames);
        } catch (AssertionError | ParseException | IOException e) {
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
