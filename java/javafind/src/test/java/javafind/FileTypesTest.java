package javafind;

import org.junit.Test;

import java.nio.file.Path;
import java.nio.file.Paths;

import static org.junit.Assert.*;

public class FileTypesTest {
    private final FileTypes fileTypes;

    public FileTypesTest() {
        fileTypes = new FileTypes();
    }

    @Test
    public final void archiveFileTest() {
        Path path = Paths.get("test.zip");
        assertEquals("zip", FileUtil.getExtension(path));
        assertTrue(fileTypes.isArchiveFile(path));
        assertFalse(fileTypes.isBinaryFile(path));
        assertFalse(fileTypes.isCodeFile(path));
        assertFalse(fileTypes.isTextFile(path));
        assertFalse(fileTypes.isXmlFile(path));
        assertEquals(FileType.ARCHIVE, fileTypes.getFileType(path));
    }

    @Test
    public final void binaryFileTest() {
        Path path = Paths.get("test.exe");
        assertEquals("exe", FileUtil.getExtension(path));
        assertFalse(fileTypes.isArchiveFile(path));
        assertTrue(fileTypes.isBinaryFile(path));
        assertFalse(fileTypes.isCodeFile(path));
        assertFalse(fileTypes.isTextFile(path));
        assertFalse(fileTypes.isXmlFile(path));
        assertEquals(FileType.BINARY, fileTypes.getFileType(path));
    }

    @Test
    public final void codeFileTest() {
        Path path = Paths.get("Test.java");
        assertEquals("java", FileUtil.getExtension(path));
        assertFalse(fileTypes.isArchiveFile(path));
        assertFalse(fileTypes.isBinaryFile(path));
        assertTrue(fileTypes.isCodeFile(path));
        assertTrue(fileTypes.isTextFile(path));
        assertFalse(fileTypes.isXmlFile(path));
        assertEquals(FileType.CODE, fileTypes.getFileType(path));
    }

    @Test
    public final void textFileTest() {
        Path path = Paths.get("test.txt");
        assertEquals("txt", FileUtil.getExtension(path));
        assertFalse(fileTypes.isArchiveFile(path));
        assertFalse(fileTypes.isBinaryFile(path));
        assertFalse(fileTypes.isCodeFile(path));
        assertTrue(fileTypes.isTextFile(path));
        assertFalse(fileTypes.isXmlFile(path));
        assertEquals(FileType.TEXT, fileTypes.getFileType(path));
    }

    @Test
    public final void xmlFileTest() {
        Path path = Paths.get("markup.xml");
        assertEquals("xml", FileUtil.getExtension(path));
        assertFalse(fileTypes.isArchiveFile(path));
        assertFalse(fileTypes.isBinaryFile(path));
        assertFalse(fileTypes.isCodeFile(path));
        assertTrue(fileTypes.isTextFile(path));
        assertTrue(fileTypes.isXmlFile(path));
        assertEquals(FileType.XML, fileTypes.getFileType(path));
    }

    @Test
    public final void unknownFileTest() {
        Path path = Paths.get("unknown.ZZZ");
        assertEquals("zzz", FileUtil.getExtension(path));
        assertFalse(fileTypes.isArchiveFile(path));
        assertFalse(fileTypes.isBinaryFile(path));
        assertFalse(fileTypes.isTextFile(path));
        assertFalse(fileTypes.isXmlFile(path));
        assertTrue(fileTypes.isUnknownFile(path));
        assertEquals(FileType.UNKNOWN, fileTypes.getFileType(path));
    }
}
