package ktfind

import java.io.File
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
        val file = File("test.zip")
        assertEquals("zip", file.extension)
        assertTrue(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertFalse(fileTypes.isCodeFile(file))
        assertFalse(fileTypes.isTextFile(file))
        assertFalse(fileTypes.isXmlFile(file))
        assertEquals(FileType.ARCHIVE, fileTypes.getFileType(file))
    }

    @Test
    fun binaryFileTest() {
        val file = File("test.exe")
        assertEquals("exe", file.extension)
        assertFalse(fileTypes.isArchiveFile(file))
        assertTrue(fileTypes.isBinaryFile(file))
        assertFalse(fileTypes.isCodeFile(file))
        assertFalse(fileTypes.isTextFile(file))
        assertFalse(fileTypes.isXmlFile(file))
        assertEquals(FileType.BINARY, fileTypes.getFileType(file))
    }

    @Test
    fun javaFileTest() {
        val file = File("Test.java")
        assertEquals("java", file.extension)
        assertFalse(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertTrue(fileTypes.isCodeFile(file))
        assertTrue(fileTypes.isTextFile(file))
        assertFalse(fileTypes.isXmlFile(file))
        assertEquals(FileType.CODE, fileTypes.getFileType(file))
    }

    @Test
    fun textFileTest() {
        val file = File("test.txt")
        assertEquals("txt", file.extension)
        assertFalse(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertFalse(fileTypes.isCodeFile(file))
        assertTrue(fileTypes.isTextFile(file))
        assertFalse(fileTypes.isXmlFile(file))
        assertEquals(FileType.TEXT, fileTypes.getFileType(file))
    }

    @Test
    fun textFileTestUppercase() {
        val file = File("TEXT.TXT")
        assertEquals("TXT", file.extension)
        assertFalse(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertFalse(fileTypes.isCodeFile(file))
        assertTrue(fileTypes.isTextFile(file))
        assertFalse(fileTypes.isXmlFile(file))
        assertEquals(FileType.TEXT, fileTypes.getFileType(file))
    }

    @Test
    fun unknownFileTest() {
        val file = File("unknown.ZZZ")
        assertEquals("ZZZ", file.extension)
        assertFalse(fileTypes.isArchiveFile(file))
        assertFalse(fileTypes.isBinaryFile(file))
        assertFalse(fileTypes.isCodeFile(file))
        assertFalse(fileTypes.isTextFile(file))
        assertFalse(fileTypes.isXmlFile(file))
        assertTrue(fileTypes.isUnknownFile(file))
        assertEquals(FileType.UNKNOWN, fileTypes.getFileType(file))
    }
}
