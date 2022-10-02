package javafind;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
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
            Object obj = new JSONParser().parse(new InputStreamReader(fileTypesInputStream));
            JSONObject jsonObj = (JSONObject)obj;
            JSONArray filetypesArray = (JSONArray) jsonObj.get("filetypes");

            for (Object o : filetypesArray) {
                Map<String, Object> filetypeMap = (Map<String, Object>) o;
                String typeName = (String) filetypeMap.get("type");
                JSONArray extArray = (JSONArray) filetypeMap.get("extensions");
                Set<String> extSet = new HashSet<String>(extArray);
                fileTypeExtMap.put(typeName, extSet);
                JSONArray nameArray = (JSONArray) filetypeMap.get("names");
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
        String lname = name.toLowerCase();
        if (lname.equals(code)) return FileType.CODE;
        if (lname.equals(xml)) return FileType.XML;
        if (lname.equals(text)) return FileType.TEXT;
        if (lname.equals(binary)) return FileType.BINARY;
        if (lname.equals(archive)) return FileType.ARCHIVE;
        return FileType.UNKNOWN;
    }

    final FileType getFileType(final File f) {
        if (isCodeFile(f)) return FileType.CODE;
        if (isXmlFile(f)) return FileType.XML;
        if (isTextFile(f)) return FileType.TEXT;
        if (isBinaryFile(f)) return FileType.BINARY;
        if (isArchiveFile(f)) return FileType.ARCHIVE;
        return FileType.UNKNOWN;
    }

    final boolean isArchiveFile(final File f) {
        return fileTypeNameMap.get(archive).contains(f.getName())
            || fileTypeExtMap.get(archive).contains(FileUtil.getExtension(f));
    }

    final boolean isBinaryFile(final File f) {
        return fileTypeNameMap.get(binary).contains(f.getName())
            || fileTypeExtMap.get(binary).contains(FileUtil.getExtension(f));
    }

    public final boolean isCodeFile(final File f) {
        return fileTypeNameMap.get(code).contains(f.getName())
            || fileTypeExtMap.get(code).contains(FileUtil.getExtension(f));
    }

    final boolean isTextFile(final File f) {
        return fileTypeNameMap.get(text).contains(f.getName())
            || fileTypeExtMap.get(text).contains(FileUtil.getExtension(f));
    }

    final boolean isUnknownFile(final File f) {
        return getFileType(f) == FileType.UNKNOWN;
    }

    public final boolean isXmlFile(final File f) {
        return fileTypeNameMap.get(xml).contains(f.getName())
            || fileTypeExtMap.get(xml).contains(FileUtil.getExtension(f));
    }
}
