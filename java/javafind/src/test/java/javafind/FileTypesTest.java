package javafind;

import org.junit.jupiter.api.Test;

import java.nio.file.Paths;

import static org.junit.jupiter.api.Assertions.*;

public class FileTypesTest {
    private final FileTypes fileTypes;

    public FileTypesTest() {
        fileTypes = new FileTypes();
    }

    @Test
    public final void archiveFileTest() {
        var path = Paths.get("test.zip");
        assertEquals("zip", FileUtil.getExtension(path));
        assertTrue(fileTypes.isArchiveFile(path));
        assertFalse(fileTypes.isAudioFile(path));
        assertFalse(fileTypes.isBinaryFile(path));
        assertFalse(fileTypes.isCodeFile(path));
        assertFalse(fileTypes.isFontFile(path));
        assertFalse(fileTypes.isImageFile(path));
        assertFalse(fileTypes.isTextFile(path));
        assertFalse(fileTypes.isVideoFile(path));
        assertFalse(fileTypes.isXmlFile(path));
        assertEquals(FileType.ARCHIVE, fileTypes.getFileType(path));
    }

    @Test
    public final void audioFileTest() {
        var path = Paths.get("music.mp3");
        assertEquals("mp3", FileUtil.getExtension(path));
        assertFalse(fileTypes.isArchiveFile(path));
        assertTrue(fileTypes.isAudioFile(path));
        assertFalse(fileTypes.isBinaryFile(path));
        assertFalse(fileTypes.isCodeFile(path));
        assertFalse(fileTypes.isFontFile(path));
        assertFalse(fileTypes.isImageFile(path));
        assertFalse(fileTypes.isTextFile(path));
        assertFalse(fileTypes.isVideoFile(path));
        assertFalse(fileTypes.isXmlFile(path));
        assertEquals(FileType.AUDIO, fileTypes.getFileType(path));
    }

    @Test
    public final void binaryFileTest() {
        var path = Paths.get("test.exe");
        assertEquals("exe", FileUtil.getExtension(path));
        assertFalse(fileTypes.isArchiveFile(path));
        assertFalse(fileTypes.isAudioFile(path));
        assertTrue(fileTypes.isBinaryFile(path));
        assertFalse(fileTypes.isCodeFile(path));
        assertFalse(fileTypes.isFontFile(path));
        assertFalse(fileTypes.isImageFile(path));
        assertFalse(fileTypes.isTextFile(path));
        assertFalse(fileTypes.isVideoFile(path));
        assertFalse(fileTypes.isXmlFile(path));
        assertEquals(FileType.BINARY, fileTypes.getFileType(path));
    }

    @Test
    public final void codeFileTest() {
        var path = Paths.get("Test.java");
        assertEquals("java", FileUtil.getExtension(path));
        assertFalse(fileTypes.isArchiveFile(path));
        assertFalse(fileTypes.isAudioFile(path));
        assertFalse(fileTypes.isBinaryFile(path));
        assertTrue(fileTypes.isCodeFile(path));
        assertFalse(fileTypes.isFontFile(path));
        assertFalse(fileTypes.isImageFile(path));
        assertTrue(fileTypes.isTextFile(path));
        assertFalse(fileTypes.isVideoFile(path));
        assertFalse(fileTypes.isXmlFile(path));
        assertEquals(FileType.CODE, fileTypes.getFileType(path));
    }

    @Test
    public final void fontFileTest() {
        var path = Paths.get("font.ttf");
        assertEquals("ttf", FileUtil.getExtension(path));
        assertFalse(fileTypes.isArchiveFile(path));
        assertFalse(fileTypes.isAudioFile(path));
        assertFalse(fileTypes.isBinaryFile(path));
        assertFalse(fileTypes.isCodeFile(path));
        assertTrue(fileTypes.isFontFile(path));
        assertFalse(fileTypes.isImageFile(path));
        assertFalse(fileTypes.isTextFile(path));
        assertFalse(fileTypes.isVideoFile(path));
        assertFalse(fileTypes.isXmlFile(path));
        assertEquals(FileType.FONT, fileTypes.getFileType(path));
    }

    @Test
    public final void imageFileTest() {
        var path = Paths.get("Test.png");
        assertEquals("png", FileUtil.getExtension(path));
        assertFalse(fileTypes.isArchiveFile(path));
        assertFalse(fileTypes.isAudioFile(path));
        assertFalse(fileTypes.isBinaryFile(path));
        assertFalse(fileTypes.isCodeFile(path));
        assertFalse(fileTypes.isFontFile(path));
        assertTrue(fileTypes.isImageFile(path));
        assertFalse(fileTypes.isTextFile(path));
        assertFalse(fileTypes.isVideoFile(path));
        assertFalse(fileTypes.isXmlFile(path));
        assertEquals(FileType.IMAGE, fileTypes.getFileType(path));
    }

    @Test
    public final void textFileTest() {
        var path = Paths.get("test.txt");
        assertEquals("txt", FileUtil.getExtension(path));
        assertFalse(fileTypes.isArchiveFile(path));
        assertFalse(fileTypes.isAudioFile(path));
        assertFalse(fileTypes.isBinaryFile(path));
        assertFalse(fileTypes.isCodeFile(path));
        assertFalse(fileTypes.isFontFile(path));
        assertFalse(fileTypes.isImageFile(path));
        assertTrue(fileTypes.isTextFile(path));
        assertFalse(fileTypes.isVideoFile(path));
        assertFalse(fileTypes.isXmlFile(path));
        assertEquals(FileType.TEXT, fileTypes.getFileType(path));
    }

    @Test
    public final void videoFileTest() {
        var path = Paths.get("movie.mp4");
        assertEquals("mp4", FileUtil.getExtension(path));
        assertFalse(fileTypes.isArchiveFile(path));
        assertFalse(fileTypes.isAudioFile(path));
        assertFalse(fileTypes.isBinaryFile(path));
        assertFalse(fileTypes.isCodeFile(path));
        assertFalse(fileTypes.isFontFile(path));
        assertFalse(fileTypes.isImageFile(path));
        assertFalse(fileTypes.isTextFile(path));
        assertTrue(fileTypes.isVideoFile(path));
        assertFalse(fileTypes.isXmlFile(path));
        assertEquals(FileType.VIDEO, fileTypes.getFileType(path));
    }

    @Test
    public final void xmlFileTest() {
        var path = Paths.get("markup.xml");
        assertEquals("xml", FileUtil.getExtension(path));
        assertFalse(fileTypes.isArchiveFile(path));
        assertFalse(fileTypes.isAudioFile(path));
        assertFalse(fileTypes.isBinaryFile(path));
        assertFalse(fileTypes.isCodeFile(path));
        assertFalse(fileTypes.isFontFile(path));
        assertFalse(fileTypes.isImageFile(path));
        assertTrue(fileTypes.isTextFile(path));
        assertFalse(fileTypes.isVideoFile(path));
        assertTrue(fileTypes.isXmlFile(path));
        assertEquals(FileType.XML, fileTypes.getFileType(path));
    }

    @Test
    public final void unknownFileTest() {
        var path = Paths.get("unknown.ZZZ");
        assertEquals("zzz", FileUtil.getExtension(path));
        assertFalse(fileTypes.isArchiveFile(path));
        assertFalse(fileTypes.isAudioFile(path));
        assertFalse(fileTypes.isBinaryFile(path));
        assertFalse(fileTypes.isCodeFile(path));
        assertFalse(fileTypes.isFontFile(path));
        assertFalse(fileTypes.isImageFile(path));
        assertFalse(fileTypes.isTextFile(path));
        assertFalse(fileTypes.isVideoFile(path));
        assertFalse(fileTypes.isXmlFile(path));
        assertTrue(fileTypes.isUnknownFile(path));
        assertEquals(FileType.UNKNOWN, fileTypes.getFileType(path));
    }
}
