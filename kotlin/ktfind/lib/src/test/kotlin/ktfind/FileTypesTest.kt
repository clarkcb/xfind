package ktfind

import java.nio.file.Paths
import kotlin.io.path.extension
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * @author cary on 7/30/16.
 */
class FileTypesTest {
    private val fileTypes = FileTypes()

    @Test
    fun archiveFileTest() {
        val path = Paths.get("test.zip")
        assertEquals("zip", path.extension)
        assertTrue(fileTypes.isArchiveFile(path))
        assertFalse(fileTypes.isAudioFile(path))
        assertFalse(fileTypes.isBinaryFile(path))
        assertFalse(fileTypes.isCodeFile(path))
        assertFalse(fileTypes.isFontFile(path))
        assertFalse(fileTypes.isImageFile(path))
        assertFalse(fileTypes.isTextFile(path))
        assertFalse(fileTypes.isVideoFile(path))
        assertFalse(fileTypes.isXmlFile(path))
        assertEquals(FileType.ARCHIVE, fileTypes.getFileType(path))
    }

    @Test
    fun audioFileTest() {
        val path = Paths.get("music.mp3")
        assertEquals("mp3", path.extension)
        assertFalse(fileTypes.isArchiveFile(path))
        assertTrue(fileTypes.isAudioFile(path))
        assertFalse(fileTypes.isBinaryFile(path))
        assertFalse(fileTypes.isCodeFile(path))
        assertFalse(fileTypes.isFontFile(path))
        assertFalse(fileTypes.isImageFile(path))
        assertFalse(fileTypes.isTextFile(path))
        assertFalse(fileTypes.isVideoFile(path))
        assertFalse(fileTypes.isXmlFile(path))
        assertEquals(FileType.AUDIO, fileTypes.getFileType(path))
    }

    @Test
    fun binaryFileTest() {
        val path = Paths.get("test.exe")
        assertEquals("exe", path.extension)
        assertFalse(fileTypes.isArchiveFile(path))
        assertFalse(fileTypes.isAudioFile(path))
        assertTrue(fileTypes.isBinaryFile(path))
        assertFalse(fileTypes.isCodeFile(path))
        assertFalse(fileTypes.isFontFile(path))
        assertFalse(fileTypes.isImageFile(path))
        assertFalse(fileTypes.isTextFile(path))
        assertFalse(fileTypes.isVideoFile(path))
        assertFalse(fileTypes.isXmlFile(path))
        assertEquals(FileType.BINARY, fileTypes.getFileType(path))
    }

    @Test
    fun fontFileTest() {
        val path = Paths.get("font.ttf")
        assertEquals("ttf", path.extension)
        assertFalse(fileTypes.isArchiveFile(path))
        assertFalse(fileTypes.isAudioFile(path))
        assertFalse(fileTypes.isBinaryFile(path))
        assertFalse(fileTypes.isCodeFile(path))
        assertTrue(fileTypes.isFontFile(path))
        assertFalse(fileTypes.isImageFile(path))
        assertFalse(fileTypes.isTextFile(path))
        assertFalse(fileTypes.isVideoFile(path))
        assertFalse(fileTypes.isXmlFile(path))
        assertEquals(FileType.FONT, fileTypes.getFileType(path))
    }

    @Test
    fun imageFileTest() {
        val file = Paths.get("Test.png")
        assertEquals("png", file.extension)
        assertFalse(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isAudioFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertFalse(fileTypes.isCodeFile(file))
        assertFalse(fileTypes.isFontFile(file))
        assertTrue(fileTypes.isImageFile(file))
        assertFalse(fileTypes.isTextFile(file))
        assertFalse(fileTypes.isVideoFile(file))
        assertFalse(fileTypes.isXmlFile(file))
        assertEquals(FileType.IMAGE, fileTypes.getFileType(file))
    }

    @Test
    fun javaFileTest() {
        val file = Paths.get("Test.java")
        assertEquals("java", file.extension)
        assertFalse(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isAudioFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertTrue(fileTypes.isCodeFile(file))
        assertFalse(fileTypes.isFontFile(file))
        assertFalse(fileTypes.isImageFile(file))
        assertTrue(fileTypes.isTextFile(file))
        assertFalse(fileTypes.isVideoFile(file))
        assertFalse(fileTypes.isXmlFile(file))
        assertEquals(FileType.CODE, fileTypes.getFileType(file))
    }

    @Test
    fun textFileTest() {
        val file = Paths.get("test.txt")
        assertEquals("txt", file.extension)
        assertFalse(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isAudioFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertFalse(fileTypes.isCodeFile(file))
        assertFalse(fileTypes.isFontFile(file))
        assertFalse(fileTypes.isImageFile(file))
        assertTrue(fileTypes.isTextFile(file))
        assertFalse(fileTypes.isVideoFile(file))
        assertFalse(fileTypes.isXmlFile(file))
        assertEquals(FileType.TEXT, fileTypes.getFileType(file))
    }

    @Test
    fun textFileTestUppercase() {
        val file = Paths.get("TEXT.TXT")
        assertEquals("TXT", file.extension)
        assertFalse(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isAudioFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertFalse(fileTypes.isCodeFile(file))
        assertFalse(fileTypes.isFontFile(file))
        assertFalse(fileTypes.isImageFile(file))
        assertTrue(fileTypes.isTextFile(file))
        assertFalse(fileTypes.isVideoFile(file))
        assertFalse(fileTypes.isXmlFile(file))
        assertEquals(FileType.TEXT, fileTypes.getFileType(file))
    }

    @Test
    fun videoFileTest() {
        val file = Paths.get("movie.mp4")
        assertEquals("mp4", file.extension)
        assertFalse(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isAudioFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertFalse(fileTypes.isCodeFile(file))
        assertFalse(fileTypes.isFontFile(file))
        assertFalse(fileTypes.isImageFile(file))
        assertFalse(fileTypes.isTextFile(file))
        assertTrue(fileTypes.isVideoFile(file))
        assertFalse(fileTypes.isXmlFile(file))
        assertEquals(FileType.VIDEO, fileTypes.getFileType(file))
    }

    @Test
    fun unknownFileTest() {
        val file = Paths.get("unknown.ZZZ")
        assertEquals("ZZZ", file.extension)
        assertFalse(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isAudioFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertFalse(fileTypes.isCodeFile(file))
        assertFalse(fileTypes.isFontFile(file))
        assertFalse(fileTypes.isImageFile(file))
        assertFalse(fileTypes.isTextFile(file))
        assertFalse(fileTypes.isVideoFile(file))
        assertFalse(fileTypes.isXmlFile(file))
        assertTrue(fileTypes.isUnknownFile(file))
        assertEquals(FileType.UNKNOWN, fileTypes.getFileType(file))
    }
}
